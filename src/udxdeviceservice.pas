unit uDxDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDxDevice, fgl, DirectInput, Lua;

type
  TDxDeviceList = TFPGObjectList<TDxDevice>;
  TNewDxDeviceCallback = procedure(pDevice: TDxDevice) of object;

  TAxisHandlerInfo = class
    public
      Device: TDxDevice;
      AxisNumber: Integer;
      Interval: Integer;
      Tolerance: Integer;
      LuaRef: Integer;
      LastEventTs: Int64;
      LastEventValue: Integer;
  end;

  TAxisHandlerInfoList = TFPGObjectList<TAxisHandlerInfo>;


  { TDxDeviceService }

  TDxDeviceService = class
    private
      fDevices: TDxDeviceList;
      fDInput : IDIRECTINPUT8; //DirectInput
      fOnNewDevice: TNewDxDeviceCallback;
      fAxisHandlers: TAxisHandlerInfoList;
      procedure AddGameDevice(pDeviceName: String; Data: IDIRECTINPUTDEVICE8; pGUID: TGUID); // called by Dx Detect
      function GUID2Str(pGUID: TGUID): String;
      procedure SetBufferSize(pDevice: TDxDevice);
      procedure CheckAxisChangesInIntervalAfterPassingIntervalWindow(pDevice: TDxDevice);
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Init;
      function DetectDevices: Integer;
      procedure HandleAxisEvent(pDevice: TDxDevice; event: DIDEVICEOBJECTDATA; pAxisIndex: Integer);
      procedure TickMe;
      procedure SetAxisHandler(pDeviceName: String; pAxisNumber: Integer; pInterval: Integer; pTolerance: Integer; pHandlerRef: Integer);
      function GetButtonState(pDeviceName: String; pButtonNumber: Integer) : Integer;
      property OnNewDevice: TNewDxDeviceCallback read fOnNewDevice write fOnNewDevice;
  end;

  function LuaCmdSetAxisHandler(luaState : TLuaState) : integer;
  function LuaCmdGetButtonState(luaState : TLuaState) : integer;

implementation

uses
  uGlobals, uMainFrm, Windows, uDevice;

function EnumJoysticksCallback(const lpddi: TDIDeviceInstanceA;
  pvRef: Pointer): HRESULT; stdcall;
var logStr: String;
    DI_JoyDevice : IDIRECTINPUTDEVICE8;
    hr: HRESULT;
begin
  Result := Integer(DIENUM_CONTINUE);
  with TDxDeviceService(pvRef) do
  begin
    hr := fDInput.CreateDevice(lpddi.guidInstance, DI_JoyDevice, nil);
    if (not FAILED(hr)) then
    begin
      hr := DI_JoyDevice.SetDataFormat(c_dfDIJoystick2);
      if (not FAILED(hr)) then
      begin
        hr := DI_JoyDevice.SetCooperativeLevel(Glb.MainFormHandle, DISCL_NONEXCLUSIVE or DISCL_BACKGROUND);
        if (not FAILED(hr)) then
        begin
          logStr := lpddi.tszInstanceName;
          //logStr := lpddi.tszProductName;
          AddGameDevice(logStr, DI_JoyDevice, lpddi.guidInstance);
        end;
      end;
    end;
    //FDeviceGUID := lpddi.guidInstance;
    //Result := Integer(DIENUM_STOP);
  end;
end;

function LuaCmdSetAxisHandler(luaState: TLuaState): integer;
var
  lDeviceName : PAnsiChar;
  lAxisNumber : Integer;
  lAxisNumberString : String;
  lInterval : Integer;
  lTolerance: Integer;
  lHandlerRef: Integer;
  lNumOfParams: Integer;
begin
  // Device name
  // Axis number
  // Interval
  // Tolerance
  // handler
  lNumOfParams:=lua_gettop(luaState);
  lDeviceName := lua_tostring(luaState, 1);
  if (lNumOfParams = 5) then
  begin
    if lua_isnumber(luaState, 2) = 1 then
      lAxisNumber:= Trunc(lua_tonumber(luaState, 2))
    else if lua_isstring(luaState, 2) = 1 then
    begin
      lAxisNumberString := lua_tostring(luaState, 2);
      if (Length(lAxisNumberString) <> 1) then
        raise LmcException.Create('Wrong length of 2nd parameter. It must be 1 char');
      lAxisNumber:=Ord(lAxisNumberString[1]);
    end else
      raise LmcException.Create('Wrong type of 2nd parameter. Provide int or char.');
    lInterval:= Trunc(lua_tonumber(luaState, 3));
    lTolerance:= Trunc(lua_tonumber(luaState, 4));
    lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
    Glb.DebugLog(Format('Got function reference with key %d', [lHandlerRef]), cLoggerLua);
    Glb.DeviceService.DxDeviceService.SetAxisHandler(lDeviceName,lAxisNumber, lInterval, lTolerance, lHandlerRef);
  end else
    raise LmcException.Create('5 parameters expected: device, axis_number, interval[0], tolerance[1], handler');
  Result := 0;
end;

function LuaCmdGetButtonState(luaState: TLuaState): integer;
var arg : PAnsiChar;
  lNumOfParams: Integer;
  lIndex: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 2) then
    raise LmcException.Create('Wrong number of parameters. Provide at device name and button number.');
  arg := lua_tostring(luaState, 1);
  lIndex := lua_tointeger(luaState, 2);
  Glb.DebugLog(Format('Finding out button %d state of device %s', [lIndex, arg]), cLoggerLua);
  lua_pushinteger(luaState, Glb.DeviceService.DxDeviceService.GetButtonState(arg, lIndex));
  Result := 1;
end;


{ TDxDeviceService }

procedure TDxDeviceService.AddGameDevice(pDeviceName: String;
  Data: IDIRECTINPUTDEVICE8; pGUID: TGUID);
var
  caps: DIDEVCAPS;
  newDevice: TDxDevice;
  hr: HRESULT;
begin
  caps.dwSize := SizeOf(DIDEVCAPS);
  hr := Data.GetCapabilities(caps);
  if FAILED(hr) then
    exit;
  // add string to log
  newDevice := TDxDevice.Create;
  newDevice.SystemId := pDeviceName + ' ' + GUID2Str(pGUID);
  Glb.DebugLogFmt('Found game device: %s, %d buttons, %d axis, %d POVs.',
     [newDevice.SystemId, caps.dwButtons, caps.dwAxes, caps.dwPOVs], cLoggerDx);
  // create kbd object
  newDevice.Name:=newDevice.SystemId;
  newDevice.ButtonsCount := caps.dwButtons;
  newDevice.AxisCount := caps.dwAxes;
  newDevice.POVsCount := caps.dwPOVs;
  newDevice.DiDevice := Data;
  fDevices.Add(newDevice);
  SetBufferSize(newDevice);
  Glb.DeviceService.Devices.Add(newDevice);
  if Assigned(fOnNewDevice) then
    fOnNewDevice(newDevice);
end;

function TDxDeviceService.GUID2Str(pGUID: TGUID): String;
var I: Integer;
begin
  Result := IntToHex(pGUID.D1, 8);
  Result := Result + ':' + IntToHex(pGUID.D2, 4);
  Result := Result + ':' + IntToHex(pGUID.D3, 4) + ':';
  for I := Low(pGUID.D4) to High(pGUID.D4) do
    Result := Result + IntToHex(pGUID.D4[I], 2);
end;

procedure TDxDeviceService.SetBufferSize(pDevice: TDxDevice);
var
  dipdw : DIPROPDWORD;
begin
  dipdw.diph.dwSize := SizeOf(DIPROPDWORD);
  dipdw.diph.dwHeaderSize := SizeOf(DIPROPHEADER);
  dipdw.diph.dwObj := 0;
  dipdw.diph.dwHow := DIPH_DEVICE;
  dipdw.dwData := pDevice.BufferSize;
  if (pDevice.DIDevice.SetProperty(DIPROP_BUFFERSIZE, dipdw.diph) <> DI_OK) then
  begin
    Glb.LogError('Cannot set buffered mode for device ' + pDevice.Name, cLoggerDx);
  end else begin
    Glb.DebugLogFmt('Set buffered mode for device %s', [pDevice.Name], cLoggerDx);
  end;
end;

procedure TDxDeviceService.CheckAxisChangesInIntervalAfterPassingIntervalWindow(
  pDevice: TDxDevice);
var
  lAxisHandlerInfo: TAxisHandlerInfo;
  lNow: Int64;
  lData: Int64;
begin
  for lAxisHandlerInfo in fAxisHandlers do
  begin
    if (pDevice = lAxisHandlerInfo.Device) then
    begin
      lNow:=GetTickCount64;
      lData:=pDevice.AxisValue[lAxisHandlerInfo.AxisNumber];
      if (lNow - lAxisHandlerInfo.LastEventTs >= lAxisHandlerInfo.Interval) and
         (Abs(lAxisHandlerInfo.LastEventValue - lData) >= lAxisHandlerInfo.Tolerance) then
      begin
        Glb.DebugLogFmt('Device %s axis pending value triggerred lua handler call, axis %d, data %d', [pDevice.Name, lAxisHandlerInfo.AxisNumber, lData], cLoggerDx);
        lAxisHandlerInfo.LastEventValue:=lData;
        lAxisHandlerInfo.LastEventTs:=lNow;
        Glb.LuaEngine.CallFunctionByRef(lAxisHandlerInfo.LuaRef, lData, lNow);
      end;
    end;
  end;
end;

constructor TDxDeviceService.Create;
begin
  fDevices := TDxDeviceList.Create();
  fAxisHandlers := TAxisHandlerInfoList.Create();
end;

destructor TDxDeviceService.Destroy;
begin
  fDevices.Free;
  fAxisHandlers.Free;
  if fDInput <> nil then
    fDInput._Release;
end;

procedure TDxDeviceService.Init;
var
  hr: HRESULT;
begin
  fDInput := nil;
  hr := DirectInput8Create(GetModuleHandle(nil),DIRECTINPUT_VERSION,IID_IDirectInput8,fDInput,nil);
  if (FAILED(hr)) then
  begin
    Glb.LogError('Can not init Direct input. Error ' + IntToHex(hr, 8), cLoggerDx);
    exit;
  end;
  if (fDInput = nil) then
  begin
    Glb.LogError('Direct input initialization error', cLoggerDx);
    exit;
  end;
end;

function TDxDeviceService.DetectDevices: Integer;
begin
  fDevices.Clear;
  fDInput.EnumDevices(DI8DEVCLASS_GAMECTRL, @EnumJoysticksCallback, self, DIEDFL_ATTACHEDONLY);
  Result := fDevices.Count;
end;

procedure TDxDeviceService.HandleAxisEvent(pDevice: TDxDevice;
  event: DIDEVICEOBJECTDATA; pAxisIndex: Integer);
var
  lAxisHandlerInfo: TAxisHandlerInfo;
  lNow: Int64;
  lData: Int64;
begin
  //Glb.DebugLogFmt('Device %s axis event, axis %d, data %d', [pDevice.Name, pAxisIndex, event.dwData], cLoggerDx);
  for lAxisHandlerInfo in fAxisHandlers do
  begin
    if (pDevice = lAxisHandlerInfo.Device) and (lAxisHandlerInfo.AxisNumber = pAxisIndex) then
    begin
      lNow:=GetTickCount64;
      if (lNow - lAxisHandlerInfo.LastEventTs >= lAxisHandlerInfo.Interval) and
         (Abs(lAxisHandlerInfo.LastEventValue - event.dwData) >= lAxisHandlerInfo.Tolerance) then
      begin
        Glb.DebugLogFmt('Device %s axis event triggerred lua handler call, axis %d, data %d', [pDevice.Name, pAxisIndex, event.dwData], cLoggerDx);
        lAxisHandlerInfo.LastEventValue:=event.dwData;
        lAxisHandlerInfo.LastEventTs:=lNow;
        lData := event.dwData;
        Glb.LuaEngine.CallFunctionByRef(lAxisHandlerInfo.LuaRef, lData, event.dwTimeStamp);
      end;
    end;
  end;
end;

procedure TDxDeviceService.TickMe;
var
  lDev : TDxDevice;
begin
  for lDev in fDevices do
  begin
    lDev.ReadEventBuffer;
    CheckAxisChangesInIntervalAfterPassingIntervalWindow(lDev);
  end;
end;

procedure TDxDeviceService.SetAxisHandler(pDeviceName: String;
  pAxisNumber: Integer; pInterval: Integer; pTolerance: Integer;
  pHandlerRef: Integer);
var
  lDevice: TDevice;
  lHandler: TAxisHandlerInfo;
begin
  lDevice := Glb.DeviceService.GetByName(pDeviceName);
  if (lDevice = nil) then
    Glb.LogError('Device with name ' + pDeviceName + ' not found', cLoggerDx)
  else if (not (lDevice is TDxDevice)) then
    Glb.LogError('Device ' + pDeviceName + ' is not game device. Axis are supposed to be handled for game devices only.', cLoggerDx)
  else begin
    lHandler := TAxisHandlerInfo.Create;
    lHandler.Device := lDevice as TDxDevice;
    lHandler.AxisNumber:=pAxisNumber;
    lHandler.Interval:=pInterval;
    lHandler.Tolerance:=pTolerance;
    lHandler.LuaRef:=pHandlerRef;
    fAxisHandlers.Add(lHandler);
    Glb.DebugLog(Format('Added handler %d for axes %d of device %s, interval %d, tolerance %d',
        [pHandlerRef, pAxisNumber, lDevice.Name, pInterval, pTolerance]), cLoggerDx);
  end;
end;

function TDxDeviceService.GetButtonState(pDeviceName: String;
  pButtonNumber: Integer): Integer;
var
  lDevice: TDevice;
  lDxDevice: TDxDevice;
begin
  lDevice := Glb.DeviceService.GetByName(pDeviceName);
  if (lDevice = nil) then
    Glb.LogError('Device with name ' + pDeviceName + ' not found', cLoggerDx)
  else if (not (lDevice is TDxDevice)) then
    Glb.LogError('Device ' + pDeviceName + ' is not game device. Axis are supposed to be handled for game devices only.', cLoggerDx)
  else begin
    lDxDevice := lDevice as TDxDevice;
    Result := lDxDevice.GetButtonValue(pButtonNumber);
    Glb.DebugLog(Format('Button %d of device %s has state %d',
        [pButtonNumber, pDeviceName, Result]), cLoggerDx);
  end;
end;



end.

