unit uComDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, uComDevice, fgl;

type
  TComDeviceList = TFPGObjectList<TComDevice>;

  { TComDeviceService }

  TComDeviceService = class
    private
      fDevices: TComDeviceList;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure AddComDevice(pName: String; pComPort: String);
      procedure Init;
      procedure TickMe;
  end;


implementation

uses
  uGlobals;


{ TComDeviceService }

constructor TComDeviceService.Create;
begin
  fDevices := TComDeviceList.Create();
end;

destructor TComDeviceService.Destroy;
begin
  fDevices.Free;
end;

procedure TComDeviceService.AddComDevice(pName: String; pComPort: String);
var
  lDevice: TComDevice;
begin
  Glb.DebugLog('Adding COM device: ' + pName + ' at port ' + pComPort, cComLoggerName);
  // create kbd object
  lDevice := TComDevice.Create;
  lDevice.SystemId := pComPort;
  lDevice.Name:=pName;
  fDevices.Add(lDevice);
  Glb.DeviceService.Devices.Add(lDevice);
end;

procedure TComDeviceService.Init;
begin

end;

procedure TComDeviceService.TickMe;
begin

end;

end.

