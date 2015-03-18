unit uDxDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDxDevice, fgl, DirectInput;

type
  TDxDeviceList = TFPGObjectList<TDxDevice>;
  TNewDxDeviceCallback = procedure(pDevice: TDxDevice) of object;

  { TDxDeviceService }

  TDxDeviceService = class
    private
      fDevices: TDxDeviceList;
      fDInput : IDIRECTINPUT8; //DirectInput
      fOnNewDevice: TNewDxDeviceCallback;
      procedure AddGameDevice(pDeviceName: String; Data: IDIRECTINPUTDEVICE8; pGUID: TGUID); // called by Dx Detect
      function GUID2Str(pGUID: TGUID): String;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Init;
      function DetectDevices: Integer;
      procedure TickMe;
      property OnNewDevice: TNewDxDeviceCallback read fOnNewDevice write fOnNewDevice;
  end;

implementation

uses
  uGlobals, uMainFrm, Windows;

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
        hr := DI_JoyDevice.SetCooperativeLevel(MainForm.Handle, DISCL_NONEXCLUSIVE or DISCL_BACKGROUND);
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
  Glb.DebugLog('Found game device: ' + pDeviceName + ', no of buttons: ' + IntToStr(caps.dwButtons), cDxLoggerName);
  // create kbd object
  newDevice := TDxDevice.Create;
  newDevice.SystemId:=GUID2Str(pGUID);
  //newJoy.Name := 'Game'+IntToStr(GameDevCounter+1);
  newDevice.SystemId := pDeviceName;
  newDevice.ButtonsCount := caps.dwButtons;
  newDevice.DiDevice := Data;
  fDevices.Add(newDevice);
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

constructor TDxDeviceService.Create;
begin
  fDevices := TDxDeviceList.Create();
end;

destructor TDxDeviceService.Destroy;
begin
  fDevices.Free;
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
    Glb.LogError('Can not init Direct input. Error ' + IntToHex(hr, 8), cDxLoggerName);
    exit;
  end;
  if (fDInput = nil) then
  begin
    Glb.LogError('Direct input initialization error', cDxLoggerName);
    exit;
  end;
end;

function TDxDeviceService.DetectDevices: Integer;
begin
  fDevices.Clear;
  fDInput.EnumDevices(DI8DEVCLASS_GAMECTRL, @EnumJoysticksCallback, self, DIEDFL_ATTACHEDONLY);
  Result := fDevices.Count;
end;

procedure TDxDeviceService.TickMe;
var
  lDev : TDxDevice;
begin
  for lDev in fDevices do
    lDev.DetectChanges;
end;



end.

