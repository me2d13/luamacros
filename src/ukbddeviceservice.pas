unit uKbdDeviceService;

{$mode delphi}

interface

uses
  Classes, Windows, SysUtils, uKbdDevice, fgl;

type
  TKbdDeviceList = TFPGObjectList<TKbdDevice>;

  { TKbdDeviceService }

  TKbdDeviceService = class
    private
      fDevices: TKbdDeviceList;
      function GetMessageId(Message: Word): String;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Init;
      function DetectDevices: Integer;
      procedure OrderRawInputMessagesToBeReceived(pForHandle: HWND);
      procedure OnRawMessage(var Message: TMessage);
  end;

implementation

uses
  uRawInput, uGlobals, uDevice;

{ TKbdDeviceService }

constructor TKbdDeviceService.Create;
begin
  fDevices := TKbdDeviceList.Create();
end;

destructor TKbdDeviceService.Destroy;
begin
  fDevices.Free;
end;

procedure TKbdDeviceService.Init;
begin
end;

function TKbdDeviceService.DetectDevices: Integer;
var
  deviceCount, StrLen, TmpSize: UINT;
  pDevicesHID: PRAWINPUTDEVICELIST;
  pDevice: PRAWINPUTDEVICELIST;
  pDeviceName: PChar;
  I: Integer;
  pDeviceInfo: PRID_DEVICE_INFO;
  newKbd: TKbdDevice;
begin
  fDevices.Clear;
  pDeviceInfo := nil;
  pDevicesHID := nil;
  deviceCount := 0;
  if (GetRawInputDeviceList(nil, deviceCount, sizeof(RAWINPUTDEVICELIST)) = 0) then
  begin
    try
      GetMem(pDevicesHID, deviceCount * sizeOf(RAWINPUTDEVICELIST));
      GetMem(pDeviceInfo, sizeOf(RID_DEVICE_INFO));
      pDevice := pDevicesHID;
      GetRawInputDeviceList(pDevicesHID, deviceCount, sizeof(RAWINPUTDEVICELIST));
      begin
        // process the list
        strLen := 0;
        for I := 0 to deviceCount - 1 do
        begin
          if (GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICENAME,
              nil, StrLen) = 0) then
          begin
            GetMem(pDeviceName, StrLen + 1);
            try
              GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICENAME,
                  pDeviceName, StrLen);
              TmpSize := sizeof(RID_DEVICE_INFO);
              pDeviceInfo^.cbSize := TmpSize;
              GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICEINFO, pDeviceInfo, TmpSize);
              if (pDeviceInfo^.dwType = RIM_TYPEKEYBOARD) and (strpos(strUpper(pDeviceName), 'ROOT') = nil) then
              begin
                // create kbd object
                newKbd := TKbdDevice.Create;
                newKbd.SystemId := StrPas(pDeviceName);
                newKbd.Handle:=pDevice^.hDevice;
                Glb.DebugLogFmt('Found keyboard %s handle %d.', [newKbd.SystemId, pDevice^.hDevice], cKbdLoggerName);
                fDevices.Add(newKbd);
                Glb.DeviceService.Devices.Add(newKbd);
              end
            finally
              FreeMem(pDeviceName);
            end;
          end;
          Inc(pDevice);
        end;
      end;
    finally
      FreeMem(pDevicesHID);
      FreeMem(pDeviceInfo);
    end;
  end;
  Result := fDevices.Count;
end;

procedure TKbdDeviceService.OrderRawInputMessagesToBeReceived(pForHandle: HWND
  );
var
  ids: array[0..0] of RAWINPUTDEVICE;
begin
  ids[0].usUsagePage := 1;
  ids[0].usUsage := 6;  // keyboard
  ids[0].dwFlags := RIDEV_INPUTSINK; // + RIDEV_NOLEGACY;
  ids[0].hwndTarget := pForHandle;
  if (not RegisterRawInputDevices(@ids, 1, sizeOf(RAWINPUTDEVICE))) then
  begin
    Glb.LogError('Failed to register keyboard input messages.', cKbdLoggerName);
  end;
end;

procedure TKbdDeviceService.OnRawMessage(var Message: TMessage);
var
  pcbSize: UINT;
  buff: PRAWINPUT;
  lDev: TKbdDevice;
  lDirection: Integer;
begin
  GetRawInputData(Message.LParam, RID_INPUT, nil, pcbSize, sizeOf(RAWINPUTHEADER));
  GetMem(buff, pcbSize);
  try
    if (GetRawInputData(Message.LParam, RID_INPUT, buff,
        pcbSize, sizeOf(RAWINPUTHEADER)) = pcbSize) then
    begin
      if (buff^.header.dwType = RIM_TYPEKEYBOARD)then
      begin
        case buff^.keyboard.Message of
          WM_KEYDOWN, WM_SYSKEYDOWN: lDirection:=cDirectionDown;
          WM_KEYUP, WM_SYSKEYUP: lDirection:=cDirectionUp;
        end;

        {ValidEntry := True;
        New(NewEvent);
        NewEvent^.DeviceHandle := buff^.header.hDevice;
        NewEvent^.Time := GetMessageTime;
        NewEvent^.EventType := etKeyboard;
        case buff^.keyboard.Message of
          WM_KEYDOWN, WM_SYSKEYDOWN: NewEvent^.Direction := edDown;
          WM_KEYUP, WM_SYSKEYUP: NewEvent^.Direction := edUp;
          else
            ValidEntry := False;
        end;
        NewEvent^.Code := buff^.keyboard.VKey;
        NewEvent^.MessageId := buff^.keyboard.Message;
        if (ValidEntry) then
          eb.Add(NewEvent)
        else
          Dispose(NewEvent);
        }
        Glb.DebugLog('WM_INPUT KEYBOARD message ' + GetMessageId(buff^.keyboard.Message) +
            '. Key code: ' + IntToStr(buff^.keyboard.VKey) +
            '. Dev handle: ' + IntToStr(buff^.header.hDevice) + ', ext info:' + IntToStr(buff^.keyboard.ExtraInformation), cKbdLoggerName);
        // search device
        for lDev in fDevices do
        begin
          if (lDev.Name <> '') and (lDev.Handle = buff^.header.hDevice) then
          begin
            Glb.LuaEngine.OnDeviceEvent(lDev, buff^.keyboard.VKey, lDirection);
          end;
        end;
      end;
    end;
  finally
    FreeMem(buff);
  end;
end;

function TKbdDeviceService.GetMessageId(Message: Word): String;
begin
  case Message of
    WM_KEYDOWN:         Result := 'WM_KEYDOWN';
    WM_SYSKEYDOWN:      Result := 'WM_SYSKEYDOWN';
    WM_KEYUP:           Result := 'WM_KEYUP';
    WM_SYSKEYUP:        Result := 'WM_SYSKEYUP';
    WM_LBUTTONDOWN:     Result := 'WM_LBUTTONDOWN';
    WM_MBUTTONDOWN:     Result := 'WM_MBUTTONDOWN';
    WM_RBUTTONDOWN:     Result := 'WM_RBUTTONDOWN';
    WM_LBUTTONUP:       Result := 'WM_LBUTTONUP';
    WM_MBUTTONUP:       Result := 'WM_MBUTTONUP';
    WM_RBUTTONUP:       Result := 'WM_RBUTTONUP';
    WM_MOUSEWHEEL:      Result := 'WM_MOUSEWHEEL';
    else Result := IntToHex(Message, 4);
  end;
end;



end.

