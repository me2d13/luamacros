unit uComDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazSerial, uDevice;

type

    { TComDevice }

    TComDevice = class(TDevice)
    private
      fComPort: TLazSerial;
      function GetActive: Boolean;
      procedure OnRxData(Sender: TObject);
      procedure SetActive(AValue: Boolean);
    public
      constructor Create;
      destructor Destroy;override;
      function TypeCaption: String; override;
      procedure Init;
      property Active: Boolean read GetActive write SetActive;
  end;

const
  cComLoggerName = 'COM';


implementation

uses
  uMainFrm, uGlobals;

{ TComDevice }

procedure TComDevice.OnRxData(Sender: TObject);
var
  lData: String;
begin
  lData := fComPort.ReadData;
  Glb.DebugLog('Received data from COM port ' + SystemId + ': ' + lData, cComLoggerName);
  Glb.LuaEngine.OnDeviceEvent(self, lData);
end;

function TComDevice.GetActive: Boolean;
begin
  Result := (fComPort <> nil) and fComPort.Active;
end;

procedure TComDevice.SetActive(AValue: Boolean);
begin
  Glb.DebugLog('Setting port '+Name+' active ' + BoolToStr(AValue), cComLoggerName);
  if (fComPort = nil) then
    Init;
  if (fComPort <> nil) then
    fComPort.Active:=AValue;
end;

constructor TComDevice.Create;
begin
  fComPort := nil;
end;

destructor TComDevice.Destroy;
begin
  if (fComPort <> nil) then
    fComPort.Free;
  inherited Destroy;
end;

function TComDevice.TypeCaption: String;
begin
  Result:='COM';
end;

procedure TComDevice.Init;
begin
  fComPort := TLazSerial.Create(MainForm);
  fComPort.Device:=SystemId;
  fComPort.Active:=True;
  // callback
  fComPort.OnRxData:=OnRxData;
end;

end.

