unit uUsb;

interface

uses Windows, Messages, Classes;

type
  PDevBroadcastHdr  = ^DEV_BROADCAST_HDR;
  DEV_BROADCAST_HDR = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  TDevBroadcastHdr = DEV_BROADCAST_HDR;

type
  PDevBroadcastDeviceInterface  = ^DEV_BROADCAST_DEVICEINTERFACE;
  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: Char;
  end;
  TDevBroadcastDeviceInterface = DEV_BROADCAST_DEVICEINTERFACE;

const
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  DBT_DEVICEARRIVAL          = $8000;
  DBT_DEVICEREMOVECOMPLETE   = $8004;
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;

type
  TUsbNotifyProc = procedure(Sender: TObject; const DeviceName: String) of Object;
  TUsbNotifier = class
  private
    FWindowHandle: HWND;
    FNotificationHandle: Pointer;
    FOnUsbArrival: TUsbNotifyProc;
    FOnUsbRemoval: TUsbNotifyProc;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    property OnUsbArrival: TUsbNotifyProc read FOnUsbArrival write FOnUsbArrival;
    property OnUsbRemoval: TUsbNotifyProc read FOnUsbRemoval write FOnUsbRemoval;
    destructor Destroy; override;
  end;


implementation

uses JwaWinUser, uMainFrm;

constructor TUsbNotifier.Create;
var
  Size: Cardinal;
  Dbi: TDevBroadcastDeviceInterface;
begin
  inherited;

  Size := SizeOf(Dbi);
  //ZeroMemory(@Dbi, Size);

  Dbi.dbcc_size := Size;
  Dbi.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  Dbi.dbcc_classguid := GUID_DEVINTERFACE_USB_DEVICE;

  FNotificationHandle := RegisterDeviceNotification(gMainForm.Handle, @Dbi,
                                              DEVICE_NOTIFY_WINDOW_HANDLE);
end;

procedure TUsbNotifier.WndProc(var Msg: TMessage);
var
  Dbi: PDevBroadcastDeviceInterface;
begin
  with Msg do
  if (Msg = WM_DEVICECHANGE)
      and ((WParam = DBT_DEVICEARRIVAL)
        or (WParam = DBT_DEVICEREMOVECOMPLETE)) then
  try
    Dbi := PDevBroadcastDeviceInterface(LParam);
    if Dbi.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE then
    begin
      if WParam = DBT_DEVICEARRIVAL then
      begin
        if Assigned(FOnUsbArrival) then
          FOnUsbArrival(Self, PChar(@Dbi.dbcc_name));
      end
      else
      begin
        if Assigned(FOnUsbRemoval) then
          FOnUsbRemoval(Self, PChar(@Dbi.dbcc_name));
      end;
    end;
  except
    Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
  end
  else
    Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
end;

destructor TUsbNotifier.Destroy;
begin
  UnregisterDeviceNotification(FNotificationHandle);
  DeallocateHWnd(FWindowHandle);
  inherited;
end;

end.
