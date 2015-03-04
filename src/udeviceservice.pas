unit uDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice, fgl, uDxDeviceService, uComDeviceService;

type
  TDeviceList = TFPGObjectList<TDevice>;

  { TDeviceService }

  TDeviceService = class
    private
      fDevices: TDeviceList;
      fDxService: TDxDeviceService;
      fComService: TComDeviceService;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure ListDevices;
      procedure Init;
      function DetectDevices: Integer;
      procedure TickMe;
      procedure CheckNameAsk(pName: String);
      function AssignNameByRegexp(pName: String; pRegexp: String): String;
      function GetByName(pDeviceName:String): TDevice;
      procedure AddCom(pName:String; pPortName: String);overload;
      procedure AddCom(pName:String; pPortName: String; pSpeed: Integer; pDataBits: Integer;
        pParity: String; pStopBits: Integer);overload;
      procedure SendToCom(pDevName: String; pData: String);

      property Devices:TDeviceList read fDevices;
  end;

implementation

uses
  uGlobals, regexpr, uComDevice;

{ TDeviceService }

constructor TDeviceService.Create;
begin
  fDevices := TDeviceList.Create(False);
  fDxService := TDxDeviceService.Create;
  fComService := TComDeviceService.Create;
end;

destructor TDeviceService.Destroy;
begin
  fDxService.Free;
  fDevices.Free;
  fComService.Free;
  inherited;
end;

procedure TDeviceService.ListDevices;
var
  lItem: TDevice;
  lName: String;
begin
  for lItem in fDevices do
  begin
    if (lItem.Name = '') then
      lName := cUnassigned
    else
      lName := lItem.Name;
    Glb.Print(Format('%s  :  %s  :  %s', [lName, lItem.SystemId, lItem.TypeCaption]));
  end;
  Glb.Print('Total number of devices: ' + IntToStr(fDevices.Count));
end;

procedure TDeviceService.Init;
begin
  fDxService.Init;
  DetectDevices;
end;

function TDeviceService.DetectDevices: Integer;
begin
  fDevices.Clear;
  fDxService.DetectDevices;
  Result := fDevices.Count;
end;

procedure TDeviceService.TickMe;
begin
  fDxService.TickMe;
end;

procedure TDeviceService.CheckNameAsk(pName: String);
var
  lItem: TDevice;
begin
  // check pName is assigned to some device
  // if not - scan it
  for lItem in fDevices do
  begin
    if (UpperCase(lItem.Name) = UpperCase(pName)) then
    begin
      Glb.DebugLog('Device with name ' + pName + ' check ok, already assigned', cDeviceLoggerName);
      exit;
    end
  end;
  lItem := Glb.ScanDevice;
  if (lItem <> nil) then
  begin
    Glb.DebugLog(Format('Name %s assigned to device %s %s', [pName, lItem.TypeCaption, lItem.SystemId]), cDeviceLoggerName);
    lItem.Name:=pName;
  end;
end;

function TDeviceService.AssignNameByRegexp(pName: String; pRegexp: String
  ): String;
var
  lRe: TRegExpr;
  lItem: TDevice;
begin
  Result := '';
  lRe := TRegExpr.Create;
  try
    lRe.Expression:=pRegexp;
    for lItem in fDevices do
    begin
      if (lRe.Exec(lItem.SystemId)) then
      begin
        lItem.Name:=pName;
        Result := lItem.SystemId;
        break;
      end;
    end;
  finally
    lRe.Free;
  end;
end;

function TDeviceService.GetByName(pDeviceName: String): TDevice;
var
  lItem: TDevice;
begin
  Result := nil;
  for lItem in fDevices do
    if (UpperCase(pDeviceName) = UpperCase(lItem.Name)) then
    begin
      Result := lItem;
      break;
    end;
end;

procedure TDeviceService.AddCom(pName: String; pPortName: String);
begin
  fComService.AddComDevice(pName, pPortName);
end;

procedure TDeviceService.AddCom(pName: String; pPortName: String;
  pSpeed: Integer; pDataBits: Integer; pParity: String; pStopBits: Integer);
begin
  fComService.AddComDevice(pName, pPortName, pSpeed, pDataBits, pParity, pStopBits);
end;

procedure TDeviceService.SendToCom(pDevName: String; pData: String);
var
  lDev: TDevice;
begin
  lDev := GetByName(pDevName);
  if (lDev = nil) then
    raise TDeviceException.CreateFmt('Device %s not found.', [pDevName]);
  if not (lDev is TComDevice) then
    raise TDeviceException.CreateFmt('Device %s is not serial device, can not send data.', [pDevName]);
  (lDev as TComDevice).SendData(pData);
end;

end.

