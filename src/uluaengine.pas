unit uLuaEngine;

interface

uses
  Classes, SysUtils, Lua, uDevice, fgl, uKbdDevice, syncobjs, windows;

type

  TTrigger = class
    public
      Device: TDevice;
      KeyNumber: Integer;
      Direction: Integer;
      LuaRef: Integer;
      WholeDevice: Boolean;
  end;

  { TRunItem }

  TRunItem = class
    public
      procedure Execute(pLua: TLua); virtual;
      function Describe: String; virtual;
  end;

  { TRiCode }

  TRiCode = class (TRunItem)
    private
      fCode: String;
    public
      constructor Create(pCode: String);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TRiRef }

  TRiRef = class (TRunItem)
    protected
      fRef: Integer;
    public
      constructor Create(pRef: Integer);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TRiRefString }

  TRiRefString = class (TRiRef)
    protected
      fPar1: String;
    public
      constructor Create(pRef: Integer; p1: String);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TRiRefIntegerInteger }

  TRiRefIntegerInteger = class (TRiRef)
    protected
      fPar1: Integer;
      fPar2: Integer;
    public
      constructor Create(pRef: Integer; p1, p2: Integer);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  TRunItemList = TFPGObjectList<TRunItem>;

  TTriggerList = TFPGObjectList<TTrigger>;

  { TLuaExecutor }

  TLuaExecutor = class (TThread)
    private
      fLua: TLua;
      fRunList: TRunItemList;
      fRlSynchronizer: TMultiReadExclusiveWriteSynchronizer;
      fEvent: TEventObject;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean; pLua: TLua);
      destructor Destroy;override;
      function GetQueueSize: Integer;
      function Run(pItem: TRunItem): Integer;
      procedure Terminate;
      function IsRunning: Boolean;
  end;

  { TLuaEngine }

  TLuaEngine = class (TThread)
    private
      fLua: TLua;
      fExecutor: TLuaExecutor;
      fInitOk: boolean;
      fTriggers: TTriggerList;
      procedure RegisterFunctions;
      procedure CallFunctionByRef(pRef: Integer);overload;
      procedure CallFunctionByRef(pRef: Integer; pData: String);overload;
      procedure CallFunctionByRef(pRef: Integer; pKey: Integer; pDirection: Integer);overload;
    public
      constructor Create;
      destructor Destroy;override;
      procedure Init;
      procedure UnInit;
      procedure Reset;
      procedure RunCode(pSource: String);
      procedure SetCallback(pDeviceName: String; pButton: Integer; pDirection: Integer; pHandlerRef: Integer);
      procedure SetDeviceCallback(pDeviceName: String; pHandlerRef: Integer);
      procedure OnDeviceEvent(pDevice: TDevice; pButton: Integer; pDirection: Integer);overload;
      procedure OnDeviceEvent(pDevice: TDevice; pData: String);overload;
      function IsKeyHandled(pKsPtr: TKeyStrokePtr): boolean;
      function GetQueueSize: Integer;
      function GetExecutionQueueSize: Integer;
      function IsRunning: Boolean;
  end;

implementation

uses uMainFrm, uGlobals,
  uLuaCmdXpl, uLuaCmdDevice, uComDevice, uSendKeys;

const
{$IFDEF UNIX}
{$IFDEF CPU64}
     LUALIBRARY = 'liblua52-64.so';
{$ELSE}
     LUALIBRARY = 'liblua52-32.so';
{$ENDIF}
{$ELSE}
{$IFDEF CPU64}
     LUALIBRARY = 'lua52-64.dll';
{$ELSE}
     LUALIBRARY = 'lua52-32.dll';
{$ENDIF}
{$ENDIF}

  cMaxQueueSize = 30;

  cLuaLoggerName = 'LUA';

function FormPrint(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.print(arg); // glb version is thread safe
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function FormClear(luaState : TLuaState) : integer;
begin
     gMainForm.ClearLog;
     Result := 0;
end;

function LogModule(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.LogModule(arg);
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function LogAll(luaState : TLuaState) : integer;
begin
     Glb.LogAll:=true;
     Result := 0;
end;

function LogSpool(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.SpoolFileName := arg;
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function SendKeys(luaState : TLuaState) : integer;
var
  arg : PAnsiChar;
  lSndKey: TKeySequence;
begin
  arg := lua_tostring(luaState, 1);
  lSndKey := TKeySequence.Create;
  lSndKey.Sequence := arg;
  lSndKey.Resume;
  Lua_Pop(luaState, Lua_GetTop(luaState));
  Result := 0;
end;

{ TLuaExecutor }

procedure TLuaExecutor.Execute;
var
  lQSize: Integer;
  lStart, lStop: LongInt;
begin
  while (not Terminated) do
  begin
    lQSize:=GetQueueSize;
    if (lQSize = 0) then
    begin
      Glb.DebugLog('Lua worker: queue size is 0, suspending thread...', cLuaLoggerName);
      if (Glb.MainFormHandle <> 0) then // first start handle is not yet ready
        PostMessage(Glb.MainFormHandle, WM_LUA_RUN_CHANGE, 0, 0);
      fEvent.WaitFor(INFINITE);
      fEvent.ResetEvent;
      Glb.DebugLog('Lua worker: resumed...', cLuaLoggerName);
      PostMessage(Glb.MainFormHandle, WM_LUA_RUN_CHANGE, 0, 0);
    end;
    if (lQSize > 0) then
    begin
      Glb.DebugLogFmt('Lua worker: starting %s, queue size is %d',
          [fRunList.Items[0].Describe, lQSize], cLuaLoggerName);
      try
        lStart:=Round(Now * 24*60*60*1000);
        fRunList.Items[0].Execute(fLua);
        lStop:=Round(Now * 24*60*60*1000);
        Glb.DebugLogFmt('Lua worker: finished %s, execution time: %d ms',
            [fRunList.Items[0].Describe, lStop - lStart], cLuaLoggerName);
      except
        on E: Exception do
        begin
          lStop:=Round(Now * 24*60*60*1000);
          Glb.DebugLogFmt('Lua worker: finished %s with error, execution time: %d ms',
              [fRunList.Items[0].Describe, lStop - lStart], cLuaLoggerName);
          Glb.LogError('Exception in LUA code: ' + E.Message, cLuaLoggerName);
        end;
      end;
      fRlSynchronizer.Beginwrite;
      try
        fRunList.Delete(0);
      finally
        fRlSynchronizer.Endwrite;
      end;
    end;
  end;
end;

constructor TLuaExecutor.Create(CreateSuspended: Boolean; pLua: TLua);
begin
  inherited Create(CreateSuspended);
  fLua := pLua;
  fRunList := TRunItemList.Create();
  fRlSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  fEvent:=TEventObject.Create(nil, true, false, 'LuaExe');
end;

destructor TLuaExecutor.Destroy;
begin
  fRunList.Free;
  fRlSynchronizer.Free;
  //fEvent.Free; execute.reset can be called on freed object
  inherited Destroy;
end;

function TLuaExecutor.GetQueueSize: Integer;
begin
  fRlSynchronizer.Beginread;
  Result := fRunList.Count;
  fRlSynchronizer.Endread;
end;

function TLuaExecutor.Run(pItem: TRunItem): Integer;
begin
  if GetQueueSize > cMaxQueueSize then
    raise LmcException.Create('Maximum execution queue size reached. Make scripts faster or triggers slower.');
  fRlSynchronizer.Beginwrite;
  try
    fRunList.Add(pItem);
    Result := fRunList.Count;
  finally
    fRlSynchronizer.Endwrite;
  end;
  Glb.DebugLogFmt('Item %s added to queue making it %d items big.', [pItem.Describe, Result], cLuaLoggerName);
  fEvent.SetEvent; // even if it runs already
end;

procedure TLuaExecutor.Terminate;
begin
  // Base Terminate method (to set Terminated=true)
   TThread(self).Terminate;
   // Signal event to wake up the thread
   fEvent.SetEvent;
end;

function TLuaExecutor.IsRunning: Boolean;
begin
  Result := GetQueueSize > 0;
end;

{ TRiRefIntegerInteger }

constructor TRiRefIntegerInteger.Create(pRef: Integer; p1, p2: Integer);
begin
  inherited Create(pRef);
  fPar1:=p1;
  fPar2:=p2;
end;

procedure TRiRefIntegerInteger.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pushinteger(pLua.LuaInstance, fPar1);
  lua_pushinteger(pLua.LuaInstance, fPar2);
  lua_pcall(pLua.LuaInstance, 2, 0, LUA_MULTRET);
end;

function TRiRefIntegerInteger.Describe: String;
begin
  Result:=Format('callback id %d, int params %d, %d', [fRef, fPar1, fPar2]);
end;

{ TRiRefString }

constructor TRiRefString.Create(pRef: Integer; p1: String);
begin
  inherited Create(pRef);
  fPar1:=p1;
end;

procedure TRiRefString.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pushstring(pLua.LuaInstance, pChar(fPar1));
  lua_pcall(pLua.LuaInstance, 1, 0, LUA_MULTRET);
end;

function TRiRefString.Describe: String;
begin
  Result:=Format('callback id %d, string param %s', [fRef, fPar1]);
end;

{ TRunItem }

procedure TRunItem.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
end;

function TRunItem.Describe: String;
begin
  Result := '[no info]';
end;

{ TRiRef }

constructor TRiRef.Create(pRef: Integer);
begin
  fRef:=pRef;
end;

procedure TRiRef.Execute(pLua: TLua);
begin
  inherited;
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pcall(pLua.LuaInstance, 0, 0, LUA_MULTRET);
end;

function TRiRef.Describe: String;
begin
  Result:=Format('callback id %d', [fRef]);
end;

{ TRiCode }

constructor TRiCode.Create(pCode: String);
begin
  fCode:=pCode;
end;

procedure TRiCode.Execute(pLua: TLua);
var
  lMes: String;
  lRes: Integer;
begin
  inherited;
  try
    lRes := luaL_loadbuffer(pLua.LuaInstance,PAnsiChar(fCode), Length(fCode), 'LuaMacros script');
    if lRes <> 0 then
      raise Exception.Create('Cannot load buffer.');

    lRes := lua_pcall(pLua.LuaInstance, 0, LUA_MULTRET, 0);
    case lRes of
      LUA_ERRRUN    : raise Exception.Create('Runtime error');
      LUA_ERRMEM    : raise Exception.Create('Memory allocation error');
      LUA_ERRSYNTAX : raise Exception.Create('Syntax error');
      LUA_ERRERR    : raise Exception.Create('Error handling function failed');
    end;
  except
    on E:Exception do
    begin
      lMes := E.Message;
      lMes := lMes + #10 + #09 + lua_tostring(pLua.LuaInstance, -1);
      Glb.LogError(lMes, cLuaLoggerName);
    end;
  end;
end;

function TRiCode.Describe: String;
begin
  Result:=Format('simple code, length %d', [Length(fCode)]);
end;


{ TLuaEngine }

procedure TLuaEngine.RegisterFunctions;
begin
  if (fLua = nil) then
  begin
    Glb.LogError('Can''t register Lua functions, LUA init failed.', cLuaLoggerName);
    exit;
  end;
  // general
  fLua.RegisterFunction('print','',nil,@FormPrint);
  fLua.RegisterFunction('clear','',nil,@FormClear);
  fLua.RegisterFunction('lmc_log_module','',nil,@LogModule);
  fLua.RegisterFunction('lmc_log_spool','',nil,@LogSpool);
  fLua.RegisterFunction('lmc_log_all','',nil,@LogAll);
  fLua.RegisterFunction('lmc_send_keys','',nil,@SendKeys);
  // devices
  fLua.RegisterFunction('lmc_print_devices','',nil,@PrintDevices);
  fLua.RegisterFunction('lmc_assign_keyboard','',nil,@CheckDeviceNameWithAsk);
  fLua.RegisterFunction('lmc_device_set_name','',nil,@AssignDeviceNameByRegexp);
  fLua.RegisterFunction('lmc_set_handler','',nil,@LuaCmdSetCallback);
  // serial
  fLua.RegisterFunction('lmc_add_com','',nil,@AddCom);
  fLua.RegisterFunction('lmc_send_to_com','',nil,@SendCom);
  fLua.RegisterFunction('lmc_set_com_splitter','',nil,@SetComSplitter);
  // xpl
  fLua.RegisterFunction('lmc_xpl_command','',nil,@XplCommand);
  fLua.RegisterFunction('lmc_get_xpl_variable','',nil,@GetXplVariable);
  fLua.RegisterFunction('lmc_set_xpl_variable','',nil,@SetXplVariable);
  fLua.RegisterFunction('lmc_xpl_text','',nil,@XplDrawText);
  fLua.RegisterFunction('lmc_xpl_command_begin','',nil,@XplCommandBegin);
  fLua.RegisterFunction('lmc_xpl_command_end','',nil,@XplCommandEnd);
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer);
begin
  fExecutor.Run(TRiRef.Create(pRef));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pData: String);
begin
  fExecutor.Run(TRiRefString.Create(pRef, pData));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pKey: Integer;
  pDirection: Integer);
begin
  fExecutor.Run(TRiRefIntegerInteger.Create(pRef, pKey, pDirection));
end;

constructor TLuaEngine.Create;
begin
  //Tries loading the dynamic library file
  fInitOk := true;
  if not Lua.libLoaded then
  begin
    if not Lua.LoadLuaLibrary(LUALIBRARY) then
    begin
      Glb.LogError('Lua library could not load : ' + Lua.errorString, cLuaLoggerName);
      fInitOk:=false;
    end;
  end;
  if (fInitOk) then begin
    fLua := TLua.Create();
  end
  else
  begin
    fLua := nil;
  end;
  fExecutor := TLuaExecutor.Create(True, fLua);
  fTriggers := TTriggerList.Create();
  fExecutor.Start;
end;

destructor TLuaEngine.Destroy;
begin
  if Assigned(fLua) then
    fLua.Free;
  fTriggers.Free;
  fExecutor.Terminate;
  fExecutor.Free;
  inherited Destroy;
end;

procedure TLuaEngine.Init;
begin
  //loads the standard Lua toolkits
  luaL_openlibs(fLua.LuaInstance);
  //activates the Garbage collector on the Lua side
  lua_gc(fLua.LuaInstance, LUA_GCRESTART, 0);
  RegisterFunctions;
end;

procedure TLuaEngine.UnInit;
begin
  if (Assigned(fLua)) then
  begin
    //signals the garbage collector to start cleaning
    lua_gc(fLua.LuaInstance, LUA_GCCOLLECT, 0);
    //unregisters the function we loaded into the interpreter
    fLua.UnregisterFunctions(nil);
  end;
end;

procedure TLuaEngine.Reset;
begin
  UnInit;
  fLua.Free;
  fLua := TLua.Create();
  fTriggers.Clear;
  Init;
end;

procedure TLuaEngine.RunCode(pSource: String);
begin
  fExecutor.Run(TRiCode.Create(pSource));
end;

procedure TLuaEngine.SetCallback(pDeviceName: String; pButton: Integer;
  pDirection: Integer; pHandlerRef: Integer);
var
  lDevice: TDevice;
  lTrigger: TTrigger;
begin
  lDevice := Glb.DeviceService.GetByName(pDeviceName);
  if (lDevice = nil) then
    raise Exception('Device with name ' + pDeviceName + ' not found');
  lTrigger := TTrigger.Create;
  lTrigger.Device := lDevice;
  lTrigger.KeyNumber:=pButton;
  lTrigger.Direction:=pDirection;
  lTrigger.LuaRef:=pHandlerRef;
  lTrigger.WholeDevice:=False;
  fTriggers.Add(lTrigger);
  Glb.DebugLog(Format('Added handler %d for device %s, key %d, direction %d',
      [pHandlerRef, lDevice.Name, pButton, pDirection]), cLuaLoggerName);
end;

procedure TLuaEngine.SetDeviceCallback(pDeviceName: String; pHandlerRef: Integer
  );
var
  lDevice: TDevice;
  lTrigger: TTrigger;
begin
  lDevice := Glb.DeviceService.GetByName(pDeviceName);
  if (lDevice = nil) then
    raise Exception('Device with name ' + pDeviceName + ' not found');
  if (lDevice is TComDevice) then
  begin
    (lDevice as TComDevice).Active:=true;
  end;
  lTrigger := TTrigger.Create;
  lTrigger.Device := lDevice;
  lTrigger.WholeDevice:=True;
  lTrigger.LuaRef:=pHandlerRef;
  fTriggers.Add(lTrigger);
  Glb.DebugLog(Format('Added handler %d for whole device %s',
      [pHandlerRef, lDevice.Name]), cLuaLoggerName);
end;

procedure TLuaEngine.OnDeviceEvent(pDevice: TDevice; pButton: Integer;
  pDirection: Integer);
var
  lTrigger: TTrigger;
begin
  for lTrigger in fTriggers do
  begin
    if (pDevice = lTrigger.Device) then
    begin
      // callbacks with specific button and direction
      if (not lTrigger.WholeDevice) and (pButton = lTrigger.KeyNumber) and
      (pDirection = lTrigger.Direction) then
      begin
        Glb.DebugLog(Format('Calling handler %d for device %s, key %d, direction %d',
            [lTrigger.LuaRef, lTrigger.Device.Name, pButton, pDirection]), cLuaLoggerName);
        CallFunctionByRef(lTrigger.LuaRef);
      end;
      if (lTrigger.WholeDevice) then
      begin
        Glb.DebugLog(Format('Calling handler %d for device %s with params key %d, direction %d',
            [lTrigger.LuaRef, lTrigger.Device.Name, pButton, pDirection]), cLuaLoggerName);
        CallFunctionByRef(lTrigger.LuaRef, pButton, pDirection);
      end;
    end;
  end;
end;

procedure TLuaEngine.OnDeviceEvent(pDevice: TDevice; pData: String);
var
  lTrigger: TTrigger;
begin
  for lTrigger in fTriggers do
  begin
    if (pDevice = lTrigger.Device) and (lTrigger.WholeDevice) then
    begin
      Glb.DebugLog(Format('Calling handler %d for device %s with data %s',
          [lTrigger.LuaRef, pDevice.Name, pData]), cLuaLoggerName);
      CallFunctionByRef(lTrigger.LuaRef, pData);
    end;
  end;
end;

function TLuaEngine.IsKeyHandled(pKsPtr: TKeyStrokePtr): boolean;
var
  lTrigger: TTrigger;
begin
  Result := False;
  if (pKsPtr <> nil) and (pKsPtr^.Device <> nil) and (pKsPtr^.Device.Name <> '') then
  begin
    for lTrigger in fTriggers do
    begin
      // ignore direction on purpose
      // this function is called to determine whether key press should be blocked
      // in active application
      // for this decision we don't care if macro is defined for key press/release
      // for both event we block message to active application
      if (pKsPtr^.Device = lTrigger.Device) and
        (lTrigger.WholeDevice or (pKsPtr^.VKeyCode = lTrigger.KeyNumber)) then
      begin
        Result := True;
        break;
      end;
    end;
  end;
end;

function TLuaEngine.GetQueueSize: Integer;
begin
  Result := fExecutor.GetQueueSize;
end;

function TLuaEngine.GetExecutionQueueSize: Integer;
begin
  Result := fExecutor.GetQueueSize;
end;

function TLuaEngine.IsRunning: Boolean;
begin
  Result := fExecutor.IsRunning;
end;

end.

