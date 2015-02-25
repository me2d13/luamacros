unit uDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice, fgl, uDxDeviceService;

type
  TDeviceList = TFPGObjectList<TDevice>;

  { TDeviceService }

  TDeviceService = class
    private
      fDevices: TDeviceList;
      fDxService: TDxDeviceService;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure ListDevices;
      procedure Init;
      function DetectDevices: Integer;
      procedure TickMe;
      procedure CheckNameAsk(pName: String);

      property Devices:TDeviceList read fDevices;
  end;

implementation

uses
  uGlobals;

{ TDeviceService }

constructor TDeviceService.Create;
begin
  fDevices := TDeviceList.Create(False);
  fDxService := TDxDeviceService.Create;
end;

destructor TDeviceService.Destroy;
begin
  fDxService.Free;
  fDevices.Free;
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

end.

