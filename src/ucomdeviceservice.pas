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
      function CreateComDevice(pName: String; pComPort: String):TComDevice;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure AddComDevice(pName: String; pComPort: String);overload;
      procedure AddComDevice(pName:String; pPortName: String; pSpeed: Integer; pDataBits: Integer;
        pParity: String; pStopBits: Integer);overload;
      procedure Init;
      procedure TickMe;
  end;


implementation

uses
  uGlobals;


{ TComDeviceService }

function TComDeviceService.CreateComDevice(pName: String; pComPort: String
  ): TComDevice;
var
  lDevice: TComDevice;
begin
  lDevice := TComDevice.Create;
  lDevice.SystemId := pComPort;
  lDevice.Name:=pName;
  Result := lDevice;
end;

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
  lDevice := CreateComDevice(pName, pComPort);
  fDevices.Add(lDevice);
  Glb.DeviceService.Devices.Add(lDevice);
end;

procedure TComDeviceService.AddComDevice(pName: String; pPortName: String;
  pSpeed: Integer; pDataBits: Integer; pParity: String; pStopBits: Integer);
var
  lDevice: TComDevice;
begin
  Glb.DebugLogFmt('Adding COM device: %s at port %s with speed %d, data bits %d, parity %s and stop bits %d ',
    [pName, pPortName, pSpeed, pDataBits, pParity, pStopBits], cComLoggerName);
  lDevice := CreateComDevice(pName, pPortName);
  lDevice.Speed := pSpeed;
  lDevice.DataBits:=pDataBits;
  lDevice.Parity:=pParity;
  lDevice.StopBits:=pStopBits;
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

