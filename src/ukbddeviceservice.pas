unit uKbdDeviceService;

{$mode delphi}

interface

uses
  uRawInput, Classes, Windows, SysUtils, uKbdDevice, fgl;

type
  TKbdDeviceList = TFPGObjectList<TKbdDevice>;

  { TKbdDeviceService }

  TKbdDeviceService = class
    private
      fDevices: TKbdDeviceList;
      function GetMessageId(Message: Word): String;
      function DescribeRawMessage(rawdata: PRAWINPUT): String;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Init;
      function DetectDevices: Integer;
      procedure OrderRawInputMessagesToBeReceived(pForHandle: HWND);
      procedure OnRawMessage(var Message: TMessage);
      function GetCharFromVirtualKey(Key: Word): String;
      function ProcessWaitingRawMessages: Integer;
  end;

implementation

uses
  uGlobals, uDevice, uMainFrm;

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
                Glb.DebugLogFmt('Found keyboard %s handle %d.', [newKbd.SystemId, pDevice^.hDevice], cLoggerKbd);
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
  ids: RAWINPUTDEVICE;
begin
  ids.usUsagePage := 1;
  ids.usUsage := 6;  // keyboard
  ids.dwFlags := RIDEV_INPUTSINK; // + RIDEV_NOLEGACY;
  ids.hwndTarget := pForHandle;
  if (not RegisterRawInputDevices(@ids, 1, sizeOf(ids))) then
  begin
    Glb.LogError('Failed to register keyboard input messages.', cLoggerKbd);
  end;
end;

procedure TKbdDeviceService.OnRawMessage(var Message: TMessage);
var
  pcbSize: UINT;
  buff: PRAWINPUT;
  lDev: TKbdDevice;
  lDirection: Integer;
  lKeyStrokePtr: TKeyStrokePtr;
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
        Glb.DebugLog('RAW message: ' + DescribeRawMessage(buff), cLoggerKbd);
        lKeyStrokePtr := Glb.KeyLogService.AddRaw(buff);
        // search device
        for lDev in fDevices do
        begin
          if (lDev.Handle = buff^.header.hDevice) then
          begin
            if (lDev.Name <> '') then
            begin
              lKeyStrokePtr^.Device:=lDev;
              Glb.LuaEngine.OnDeviceEvent(lDev, buff^.keyboard.VKey, lDirection, buff^.keyboard.Flags);
            end;
            // for scanning consider only key down messages
            // key ups usually come when Ctrl+Enter is released to execute script
            if (Glb.ScanService.Scanning) and (lDirection = cDirectionDown) then
            begin
              Glb.ScanService.ScannedDevice := lDev;
              break;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(buff);
  end;
  Message.Result:=0;
end;

{
!!!!
GetKeyboardState destroys current hook setup & raw message, leads to unpredictable
characters e.g. on alt+arrow
10+ hours of investigation :-(((

function TKbdDeviceService.GetCharFromVirtualKey(Key: Word): String;
var
  keyboardState: TKeyboardState;
  asciiResult: Integer;
begin
  GetKeyboardState(keyboardState) ;

  SetLength(Result, 2) ;
  asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
  case asciiResult of
    0: Result := '';
    1: SetLength(Result, 1) ;
    2:;
    else
      Result := '';
  end;
end;}

function TKbdDeviceService.GetCharFromVirtualKey(Key: Word): String;
begin
  Result := IntToStr(Key);
end;

function TKbdDeviceService.ProcessWaitingRawMessages: Integer;
var
  lMsg: TMSG;
  lMessage: TMessage;
begin
  Result := 0;
  while PeekMessage(lMsg, Glb.MainFormHandle, WM_INPUT, WM_INPUT, PM_REMOVE) do
  begin
    lMessage.msg:=lMsg.message;
    lMessage.wParam:=lMsg.wParam;
    lMessage.lParam:=lMsg.lParam;
    OnRawMessage(lMessage);
    Inc(Result);
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

function TKbdDeviceService.DescribeRawMessage(rawdata: PRAWINPUT): String;
var
  lDirection: String;
begin
  case rawdata^.keyboard.Message of
    WM_KEYDOWN, WM_SYSKEYDOWN: lDirection:='UP';
    WM_KEYUP, WM_SYSKEYUP: lDirection:='DOWN';
  end;
  Result := Format('message %s, key code %d, extended %d, flags %d, makecode %d, direction %s, keyboard handle %d',
    [GetMessageId(rawdata^.keyboard.Message), rawdata^.keyboard.VKey,
     rawdata^.keyboard.ExtraInformation, rawdata^.keyboard.Flags, rawdata^.keyboard.MakeCode,
     lDirection, rawdata^.header.hDevice]);
end;



end.

