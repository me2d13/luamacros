unit uLuaEngine;

interface

uses
  Classes, SysUtils, Lua, uDevice, fgl, uKbdDevice, syncobjs, windows, uXplMessages, contnrs;

type

  TTrigger = class
    public
      Device: TDevice;
      KeyNumber: Integer;
      Direction: Integer;
      LuaRef: Integer;
      WholeDevice: Boolean;
  end;

  TStringArray = Array of String;

  { TLuaResult }

  TLuaResult = class
    private
      fResult : TStringArray;
    public
      procedure AddString(pValue: String);
      property Value: TStringArray read fResult;
  end;

  { TFuncItem }

  TFuncItem = class
    protected
      fEvent: TEventObject;
      fResult: TLuaResult;
      procedure SignalResultReady;
    public
      constructor Create;
      destructor Destroy;override;
      procedure Execute(pLua: TLua); virtual;
      function Describe: String; virtual;
      procedure WaitForResult;
      property Result: TLuaResult read fResult;
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
      fPar1: Int64;
      fPar2: Int64;
    public
      constructor Create(pRef: Integer; p1, p2: Int64);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TRiRefIntegerIntegerInt64 }

  TRiRefIntegerIntegerInt64 = class (TRiRefIntegerInteger)
    protected
      fPar3: Int64;
    public
      constructor Create(pRef: Integer; p1, p2: Integer; p3: Int64);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  TRiRefInteger = class (TRiRef)
    protected
      fPar1: Integer;
    public
      constructor Create(pRef: Integer; p1: Integer);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TRiRefXplValueInteger }

  TRiRefXplValueInteger = class (TRiRef)
    protected
      fPar1: PXplValue;
      fPar2: Integer;
      procedure PushXplValue(pLuaState: TLuaState; pValue: PXplValue);
    public
      constructor Create(pRef: Integer; p1: PXplValue; p2: Integer);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;

  { TFiRefString }

  TFiRefString = class (TFuncItem)
    protected
      fPar1: String;
      fRef: Integer;
    public
      constructor Create(pRef: Integer; p1: String);
      procedure Execute(pLua: TLua); override;
      function Describe: String; override;
  end;


  TTriggerList = TFPGObjectList<TTrigger>;

  { TLuaExecutor }

  TLuaExecutor = class (TThread)
    private
      fLua: TLua;
      fRunList: TFPObjectList;
      fRlSynchronizer: TMultiReadExclusiveWriteSynchronizer;
      fEvent: TEventObject;
      fExecutionsCount: Int64;
      fExecutionsTime: Int64;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean; pLua: TLua);
      destructor Destroy;override;
      function GetQueueSize: Integer;
      function Run(pItem: TRunItem): Integer; overload;
      function Run(pItem: TFuncItem): TLuaResult; overload;
      procedure Terminate;
      function IsRunning: Boolean;
      property ExecutionsCount: Int64 read fExecutionsCount;
      property ExecutionsTime: Int64 read fExecutionsTime;
  end;

  { TLuaEngine }

  TLuaEngine = class
    private
      fLua: TLua;
      fExecutor: TLuaExecutor;
      fInitOk: boolean;
      fTriggers: TTriggerList;
      fScriptToRun: String;
      function GetExecutionsCount: Int64;
      function GetExecutionsTime: Int64;
      procedure RegisterFunctions;
      procedure RegisterConfig;
      procedure CallFunctionByRef(pRef: Integer);overload;
      procedure CallFunctionByRef(pRef: Integer; pKey: Int64; pDirection: Int64; pTimeStamp: Int64);overload;
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
      procedure OnDeviceEvent(pDevice: TDevice; pButton: Integer; pDirection: Integer; pTimeStamp: Int64);overload;
      procedure OnDeviceEvent(pDevice: TDevice; pData: String);overload;
      function IsKeyHandled(pKsPtr: TKeyStrokePtr): boolean;
      function GetQueueSize: Integer;
      function IsRunning: Boolean;
      procedure StackDump(pLuaState: TLuaState);
      procedure CallFunctionByRef(pRef: Integer; pValue: PXplValue; pChangeCount: Integer);overload;
      procedure CallFunctionByRef(pRef: Integer; pData: String);overload;
      procedure CallFunctionByRef(pRef: Integer; pData: Integer);overload;
      procedure CallFunctionByRef(pRef: Integer; pKey: Int64; pDirection: Int64);overload;
      function CallFunctionByRefWithResult(pRef: Integer; pData: String):TLuaResult;
      property ScriptToRun: String read fScriptToRun write fScriptToRun;
      property ExecutionsCount: Int64 read GetExecutionsCount;
      property ExecutionsTime: Int64 read GetExecutionsTime;
  end;

implementation

uses uMainFrm, uGlobals,
  uLuaCmdXpl, uLuaCmdDevice, uComDevice, uLuaCmdMainWindow, uConfigService,
  uLuaCmdHttp, uDxDeviceService;

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

{ TRiRefIntegerIntegerInt64 }

constructor TRiRefIntegerIntegerInt64.Create(pRef: Integer; p1, p2: Integer;
  p3: Int64);
begin
  inherited Create(pRef, p1, p2);
  fPar3:=p3;
end;

procedure TRiRefIntegerIntegerInt64.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pushinteger(pLua.LuaInstance, fPar1);
  lua_pushinteger(pLua.LuaInstance, fPar2);
  lua_pushinteger(pLua.LuaInstance, fPar3);
  lua_pcall(pLua.LuaInstance, 3, 0, LUA_MULTRET);
end;

function TRiRefIntegerIntegerInt64.Describe: String;
begin
  Result:=Format('callback id %d, int params %d, %d, %d', [fRef, fPar1, fPar2, fPar3]);
end;

{ TRiRefInteger }

constructor TRiRefInteger.Create(pRef: Integer; p1: Integer);
begin
  inherited Create(pRef);
  fPar1:=p1;
end;

procedure TRiRefInteger.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pushinteger(pLua.LuaInstance, fPar1);
  lua_pcall(pLua.LuaInstance, 1, 0, LUA_MULTRET);
end;

function TRiRefInteger.Describe: String;
begin
  Result:=Format('callback id %d, int param %d', [fRef, fPar1]);
end;

{ TLuaResult }

procedure TLuaResult.AddString(pValue: String);
begin
  SetLength(fResult, Length(fResult) + 1);
  fResult[Length(fResult) - 1] := pValue;
end;

{ TFiRefString }

constructor TFiRefString.Create(pRef: Integer; p1: String);
begin
  inherited Create;
  fPar1:=p1;
  fRef:=pRef;
end;

procedure TFiRefString.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  lua_pushstring(pLua.LuaInstance, pChar(fPar1));
  lua_pcall(pLua.LuaInstance, 1, 2, LUA_MULTRET);

  Glb.LuaEngine.StackDump(pLua.LuaInstance);

  fResult := TLuaResult.Create;
  if (lua_isstring(pLua.LuaInstance, -1) = 1) then
  begin
    fResult.AddString(lua_tostring(pLua.LuaInstance, -1));
  end;
  lua_pop(pLua.LuaInstance, 1);
  if (lua_isstring(pLua.LuaInstance, -1) = 1) then
  begin
    fResult.AddString(lua_tostring(pLua.LuaInstance, -1));
  end;
  lua_pop(pLua.LuaInstance, 1);
  SignalResultReady;
end;

function TFiRefString.Describe: String;
begin
  Result:=Format('function id %d, string param %s', [fRef, fPar1]);
end;

{ TFuncItem }

procedure TFuncItem.SignalResultReady;
begin
  fEvent.SetEvent;
end;

constructor TFuncItem.Create;
begin
  fEvent := TEventObject.Create(nil, true, false, 'LuaFunc' + IntToStr(Glb.UnixTimestampMs));
end;

destructor TFuncItem.Destroy;
begin
  fEvent.Free;
  // result will be freed by calling thread
  inherited Destroy;
end;

procedure TFuncItem.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
end;

function TFuncItem.Describe: String;
begin
  Result := '[no info]';
end;

procedure TFuncItem.WaitForResult;
begin
  fEvent.WaitFor(INFINITE);
end;

{ TRiRefXplValueInteger }

procedure TRiRefXplValueInteger.PushXplValue(pLuaState: TLuaState; pValue: PXplValue);
var
  lStrValue: String;
begin
  if (pValue = nil) or (pValue^.VarType = vtNull) then
  begin
    lua_pushinteger(pLuaState, 0);
    Glb.LogError('No XPL variable value for LUA callback', cLoggerLua);
  end else begin
    case pValue.VarType of
      vtInteger: lua_pushinteger(pLuaState, pValue^.intData);
      vtDouble: lua_pushnumber(pLuaState, pValue^.doubleData);
      vtString:
        begin
          lStrValue:=pValue^.stringData;
          lua_pushstring(pLuaState, PChar(lStrValue));
        end;
    end;
  end;
end;

constructor TRiRefXplValueInteger.Create(pRef: Integer; p1: PXplValue;
  p2: Integer);
begin
  inherited Create(pRef);
  fPar1:=p1;
  fPar2:=p2;
end;

procedure TRiRefXplValueInteger.Execute(pLua: TLua);
begin
  if (pLua = nil) then
    raise LmcException.Create('LUA not initialized');
  lua_rawgeti(pLua.LuaInstance, LUA_REGISTRYINDEX, fRef);
  PushXplValue(pLua.LuaInstance, fPar1);
  lua_pushinteger(pLua.LuaInstance, fPar2);
  lua_pcall(pLua.LuaInstance, 2, 0, LUA_MULTRET);
end;

function TRiRefXplValueInteger.Describe: String;
begin
  Result:=Format('callback id %d, xpl value ..., int param %d', [fRef, fPar2]);
end;

{ TLuaExecutor }

procedure TLuaExecutor.Execute;
var
  lQSize: Integer;
  lStart, lStop: LongInt;
  lProc: TRunItem;
  lFunc: TFuncItem;
  lResult: TLuaResult;
  lDesc: String;
begin
  while (not Terminated) do
  begin
    lQSize:=GetQueueSize;
    if (lQSize = 0) then
    begin
      Glb.DebugLog('Lua worker: queue size is 0, suspending thread...', cLoggerLua);
      if (Glb.MainFormHandle <> 0) then // first start handle is not yet ready
        PostMessage(Glb.MainFormHandle, WM_LUA_RUN_CHANGE, 0, 0);
      fEvent.WaitFor(INFINITE);
      fEvent.ResetEvent;
      Glb.DebugLog('Lua worker: resumed...', cLoggerLua);
      PostMessage(Glb.MainFormHandle, WM_LUA_RUN_CHANGE, 0, 0);
    end;
    if (lQSize > 0) then
    begin
      if (fRunList.Items[0] is TRunItem) then
      begin
        lProc := fRunList.Items[0] as TRunItem;
        lFunc := nil;
        lDesc := lProc.Describe;
        Glb.DebugLogFmt('Lua worker: starting procedure %s, queue size is %d',
            [lDesc, lQSize], cLoggerLua);
      end else if (fRunList.Items[0] is TFuncItem) then
      begin
        lProc := nil;
        lFunc := fRunList.Items[0] as TFuncItem;
        lDesc := lFunc.Describe;
        Glb.DebugLogFmt('Lua worker: starting function %s, queue size is %d',
            [lDesc, lQSize], cLoggerLua);
      end;
      try
        lStart:=Round(Now * 24*60*60*1000);
        if (lProc <> nil) then lProc.Execute(fLua) else
        if (lFunc <> nil) then lFunc.Execute(fLua);
        lStop:=Round(Now * 24*60*60*1000);
        Glb.DebugLogFmt('Lua worker: finished %s, execution time: %d ms',
            [lDesc, lStop - lStart], cLoggerLua);
        Inc(fExecutionsCount);
        Inc(fExecutionsTime, lStop - lStart);
        if (lProc <> nil) then lProc.Free;
      except
        on E: Exception do
        begin
          lStop:=Round(Now * 24*60*60*1000);
          Glb.DebugLogFmt('Lua worker: finished %s with error, execution time: %d ms',
              [lDesc, lStop - lStart], cLoggerLua);
          Glb.LogError('Exception in LUA code: ' + E.Message, cLoggerLua);
          if (lProc <> nil) then lProc.Free;
        end;
      end;
      fRlSynchronizer.Beginwrite;
      try
        fRunList.Delete(0);
        if (Glb.LuaEngine.ScriptToRun > '') then
        begin
          // do not run anything else, terminate and signal load to main window
          fRunList.Clear;
          PostMessage(Glb.MainFormHandle, WM_MAIN_WINDOW_COMMAND, MWC_LOAD, 0);
        end;
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
  fExecutionsTime:=0;
  fExecutionsCount:=0;
  fRunList := TFPObjectList.Create(False);
  fRlSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  fEvent:=TEventObject.Create(nil, true, false, 'LuaExe');
  FreeOnTerminate:=False;
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
  Glb.DebugLogFmt('Item %s added to queue making it %d items big.', [pItem.Describe, Result], cLoggerLua);
  fEvent.SetEvent; // even if it runs already
end;

function TLuaExecutor.Run(pItem: TFuncItem): TLuaResult;
var
  lSize: Integer;
begin
  if GetQueueSize > cMaxQueueSize then
    raise LmcException.Create('Maximum execution queue size reached. Make scripts faster or triggers slower.');
  fRlSynchronizer.Beginwrite;
  try
    fRunList.Add(pItem);
    lSize := fRunList.Count;
  finally
    fRlSynchronizer.Endwrite;
  end;
  Glb.DebugLogFmt('Item %s added to queue making it %d items big. Waiting for result.', [pItem.Describe, lSize], cLoggerLua);
  fEvent.SetEvent; // even if it runs already
  pItem.WaitForResult;
  Result := pItem.Result;
  pItem.Free;
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

constructor TRiRefIntegerInteger.Create(pRef: Integer; p1, p2: Int64);
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
      Glb.LogError(lMes, cLoggerLua);
    end;
  end;
end;

function TRiCode.Describe: String;
begin
  Result:=Format('simple code, length %d', [Length(fCode)]);
end;


{ TLuaEngine }

procedure TLuaEngine.RegisterConfig;
begin
  lua_createtable(fLua.LuaInstance, 0, 0); // 1.. table
  lua_createtable(fLua.LuaInstance, 0, 2); // 1.. table, 2 .. metatable
  lua_pushcfunction(fLua.LuaInstance, @GetParam); // 1.. table, 2 .. metatable, 3.. getter
  lua_setfield(fLua.LuaInstance, -2, '__index');  // 1.. table, 2 .. metatable
  lua_pushcfunction(fLua.LuaInstance, @SetParam); // 1.. table, 2 .. metatable, 3.. setter
  lua_setfield(fLua.LuaInstance, -2, '__newindex');  // 1.. table, 2 .. metatable
  lua_setmetatable(fLua.LuaInstance, -2);
  lua_setglobal(fLua.LuaInstance, 'lmc');
end;

procedure TLuaEngine.StackDump(pLuaState: TLuaState);
var
  i, top, lType: Integer;
begin
  top := lua_gettop(pLuaState);
  Glb.DebugLog(Format('Dumping stack with %d items.', [top]), cLoggerLua);
  for i := 1 to top do
  begin
    lType := lua_type(pLuaState, i);
    case lType of
      LUA_TSTRING: Glb.DebugLog(Format('  %d: string %s', [i, lua_tostring(pLuaState, i)]), cLoggerLua);
      LUA_TNUMBER: Glb.DebugLog(Format('  %d: number %f', [i, lua_tonumber(pLuaState, i)]), cLoggerLua);
      LUA_TFUNCTION: Glb.DebugLog(Format('  %d: function', [i]), cLoggerLua);
    else
      Glb.DebugLog(Format('  %d: unknown [%d]', [i, lType]), cLoggerLua);
    end;
  end;
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer);
begin
  fExecutor.Run(TRiRef.Create(pRef));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pData: String);
begin
  fExecutor.Run(TRiRefString.Create(pRef, pData));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pData: Integer);
begin
  fExecutor.Run(TRiRefInteger.Create(pRef, pData));
end;

function TLuaEngine.CallFunctionByRefWithResult(pRef: Integer; pData: String
  ): TLuaResult;
begin
  Glb.DebugLog('Asking executor to call a function', cLoggerLua);
  Result := fExecutor.Run(TFiRefString.Create(pRef, pData));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pKey: Int64;
  pDirection: Int64);
begin
  fExecutor.Run(TRiRefIntegerInteger.Create(pRef, pKey, pDirection));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer; pKey: Int64;
  pDirection: Int64; pTimeStamp: Int64);
begin
  fExecutor.Run(TRiRefIntegerIntegerInt64.Create(pRef, pKey, pDirection, pTimeStamp));
end;

procedure TLuaEngine.CallFunctionByRef(pRef: Integer;
  pValue: PXplValue; pChangeCount: Integer);
begin
  fExecutor.Run(TRiRefXplValueInteger.Create(pRef, pValue, pChangeCount));
end;

constructor TLuaEngine.Create;
begin
  //Tries loading the dynamic library file
  fInitOk := true;
  if not Lua.libLoaded then
  begin
    if not Lua.LoadLuaLibrary(LUALIBRARY) then
    begin
      Glb.LogError('Lua library could not load : ' + Lua.errorString, cLoggerLua);
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
  fExecutor.WaitFor;
  fExecutor.Free;
  inherited Destroy;
end;

procedure TLuaEngine.Init;
begin
  //loads the standard Lua toolkits
  luaL_openlibs(fLua.LuaInstance);
  //activates the Garbage collector on the Lua side
  lua_gc(fLua.LuaInstance, LUA_GCRESTART, 0);
  fExecutor.Start;
  RegisterFunctions;
  RegisterConfig;
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
    Glb.LogError('Device with name ' + pDeviceName + ' not found', cLoggerLua)
  else
  begin
    lTrigger := TTrigger.Create;
    lTrigger.Device := lDevice;
    lTrigger.KeyNumber:=pButton;
    lTrigger.Direction:=pDirection;
    lTrigger.LuaRef:=pHandlerRef;
    lTrigger.WholeDevice:=False;
    fTriggers.Add(lTrigger);
    Glb.DebugLog(Format('Added handler %d for device %s, key %d, direction %d',
        [pHandlerRef, lDevice.Name, pButton, pDirection]), cLoggerLua);
  end;
end;

procedure TLuaEngine.SetDeviceCallback(pDeviceName: String; pHandlerRef: Integer
  );
var
  lDevice: TDevice;
  lTrigger: TTrigger;
begin
  lDevice := Glb.DeviceService.GetByName(pDeviceName);
  if (lDevice = nil) then
    Glb.LogError('Device with name ' + pDeviceName + ' not found', cLoggerLua)
    //raise Exception('Device with name ' + pDeviceName + ' not found');
  else
  begin
    if (lDevice is TComDevice) then
    begin
      try
      (lDevice as TComDevice).Active:=true;
      except
        On Exception do begin
          // handle exception to not stop LUA code execution
          Glb.LogError('Cannot activate COM port', cLoggerCom);
          exit;
        end;
      end;
    end;
    lTrigger := TTrigger.Create;
    lTrigger.Device := lDevice;
    lTrigger.WholeDevice:=True;
    lTrigger.LuaRef:=pHandlerRef;
    fTriggers.Add(lTrigger);
    Glb.DebugLog(Format('Added handler %d for whole device %s',
        [pHandlerRef, lDevice.Name]), cLoggerLua);
  end;
end;

procedure TLuaEngine.OnDeviceEvent(pDevice: TDevice; pButton: Integer;
  pDirection: Integer);
begin
  OnDeviceEvent(pDevice, pButton, pDirection, 0);
end;

procedure TLuaEngine.OnDeviceEvent(pDevice: TDevice; pButton: Integer;
  pDirection: Integer; pTimeStamp: Int64);
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
        Glb.DebugLog(Format('Calling handler %d for device %s, key %d, direction %d, ts %d',
            [lTrigger.LuaRef, lTrigger.Device.Name, pButton, pDirection, pTimeStamp]), cLoggerLua);
        CallFunctionByRef(lTrigger.LuaRef);
      end;
      if (lTrigger.WholeDevice) then
      begin
        Glb.DebugLog(Format('Calling handler %d for device %s with params key %d, direction %d, ts %d',
            [lTrigger.LuaRef, lTrigger.Device.Name, pButton, pDirection, pTimeStamp]), cLoggerLua);
        if (pTimeStamp > 0) then
          CallFunctionByRef(lTrigger.LuaRef, pButton, pDirection, pTimeStamp)
        else
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
          [lTrigger.LuaRef, pDevice.Name, pData]), cLoggerLua);
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

function TLuaEngine.IsRunning: Boolean;
begin
  Result := fExecutor.IsRunning;
end;

procedure TLuaEngine.RegisterFunctions;
begin
  if (fLua = nil) then
  begin
    Glb.LogError('Can''t register Lua functions, LUA init failed.', cLoggerLua);
    exit;
  end;
  // general
  fLua.RegisterFunction('print','',nil,@FormPrint);
  fLua.RegisterFunction('clear','',nil,@FormClear);
  fLua.RegisterFunction('lmc_log_module','',nil,@LogModule);
  fLua.RegisterFunction('lmc_log_spool','',nil,@LogSpool);
  fLua.RegisterFunction('lmc_log_all','',nil,@LogAll);
  fLua.RegisterFunction('lmc_send_keys','',nil,@SendKeys);
  fLua.RegisterFunction('lmc_send_input','',nil,@SendInput);
  fLua.RegisterFunction('lmc_spawn','',nil,@Spawn);
  fLua.RegisterFunction('lmc_minimize','',nil,@MinimizeMainWindow);
  fLua.RegisterFunction('lmc_load','',nil,@LoadScript);
  fLua.RegisterFunction('lmc_say','',nil,@Say);
  fLua.RegisterFunction('lmc_get_window_title', '', nil, @GetActiveWindowTitle);
  fLua.RegisterFunction('lmc_sleep','',nil,@DoSleep);
  // devices
  fLua.RegisterFunction('lmc_print_devices','',nil,@PrintDevices);
  fLua.RegisterFunction('lmc_get_devices','',nil,@GetDevices);
  fLua.RegisterFunction('lmc_assign_keyboard','',nil,@CheckDeviceNameWithAsk);
  fLua.RegisterFunction('lmc_device_set_name','',nil,@AssignDeviceNameByRegexp);
  fLua.RegisterFunction('lmc_set_handler','',nil,@LuaCmdSetCallback);
  fLua.RegisterFunction('lmc_set_axis_handler','',nil,@LuaCmdSetAxisHandler);
  fLua.RegisterFunction('lmc_get_button','',nil,@LuaCmdGetButtonState);
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
  fLua.RegisterFunction('lmc_on_xpl_var_change','',nil,@XplVarChange);
  fLua.RegisterFunction('lmc_remove_xpl_var_change','',nil,@UnregisterXplVarChange);
  fLua.RegisterFunction('lmc_xpl_log','',nil,@XplLogCommand);
  fLua.RegisterFunction('lmc_inc_xpl_variable','',nil,@IncXplVariable);
  fLua.RegisterFunction('lmc_inc_xpl_array_variable','',nil,@IncXplArrayVariable);
  // http
  fLua.RegisterFunction('lmc_http_server','',nil,@HttpServerSimple);
  fLua.RegisterFunction('lmc_http_get','',nil,@HttpGet);
end;

function TLuaEngine.GetExecutionsCount: Int64;
begin
  Result := fExecutor.ExecutionsCount;
end;

function TLuaEngine.GetExecutionsTime: Int64;
begin
  Result := fExecutor.ExecutionsTime;
end;


end.

