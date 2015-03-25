unit uLuaEngine;

interface

uses
  Classes, SysUtils, Lua, uDevice, fgl, uKbdDevice;

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
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean; pLua: TLua);
      destructor Destroy;override;
      function GetQueueSize: Integer;
      function Run(pItem: TRunItem): Integer;
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
  uLuaCmdXpl, uLuaCmdDevice, uComDevice, uSendKeys, windows;

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
     MainForm.print(arg);
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function FormClear(luaState : TLuaState) : integer;
begin
     MainForm.ClearLog;
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
begin
  while (not Terminated) do
  begin
    if (GetQueueSize = 0) then
    begin
      PostMessage(MainForm.Handle, WM_LUA_RUN_CHANGE, 0, 0);
      Suspend;
      PostMessage(MainForm.Handle, WM_LUA_RUN_CHANGE, 0, 0);
    end;
    if (GetQueueSize > 0) then
    begin
      try
        fRunList.Items[0].Execute(fLua);
      except
        on E: Exception do
          Glb.LogError('Exception in LUA code: ' + E.Message, cLuaLoggerName);
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
end;

destructor TLuaExecutor.Destroy;
begin
  fRunList.Free;
  fRlSynchronizer.Free;
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
  Glb.DebugLog('Scheduled run of ' + pItem.Describe, cLuaLoggerName);
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
  fLua.RegisterFunction('lmc_log_all','',nil,@LogAll);
  fLua.RegisterFunction('lmc_send_keys','',nil,@SendKeys);
  // devices
  fLua.RegisterFunction('lmc_print_devices','',nil,@PrintDevices);
  fLua.RegisterFunction('lmc_device_name_check_ask','',nil,@CheckDeviceNameWithAsk);
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
  if (fExecutor.Suspended) then
    fExecutor.Resume;
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pData: String);
begin
  fExecutor.Run(TRiRefString.Create(pRef, pData));
  if (fExecutor.Suspended) then
    fExecutor.Resume;
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pKey: Integer;
  pDirection: Integer);
begin
  fExecutor.Run(TRiRefIntegerInteger.Create(pRef, pKey, pDirection));
  if (fExecutor.Suspended) then
    fExecutor.Resume;
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
end;

destructor TLuaEngine.Destroy;
begin
  if Assigned(fLua) then
    fLua.Free;
  fTriggers.Free;
  fExecutor.Terminate;
  if (fExecutor.Suspended) then
    fExecutor.Resume;
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
  if (fExecutor.Suspended) then
    fExecutor.Resume;
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
  Result := not fExecutor.Suspended;
end;

end.

