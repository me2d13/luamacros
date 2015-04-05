unit uScanService;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, uDevice;

type

  { TScanService }

  TScanService = class
  private
    fScanEvent: TEventObject;
    fScannedDevice: TDevice;
    fScanning: Boolean;
    fScannedName: String;
    procedure SetScannedDevice(AValue: TDevice);
  public
    constructor Create;
    destructor Destroy;override;
    function ScanDevice(pName: String): TDevice;

    property ScanEvent: TEventObject read fScanEvent;
    property ScannedDevice: TDevice read fScannedDevice write SetScannedDevice;
    property Scanning: Boolean read fScanning;
    property ScannedName: String read fScannedName;
  end;



implementation

uses
  uGlobals, windows;

{ TScanService }

procedure TScanService.SetScannedDevice(AValue: TDevice);
begin
  if fScannedDevice=AValue then Exit;
  fScannedDevice:=AValue;
  fScanning:=False;
  fScanEvent.SetEvent;
  if (Glb.MainFormHandle > 0) then
    PostMessage(Glb.MainFormHandle, WM_SCANNING_STATUS_CHANGE, 0, 0);
end;

constructor TScanService.Create;
begin
  fScanning:=false;
  fScanEvent:=TEventObject.Create(nil, true, false, 'LmcScan');
end;

destructor TScanService.Destroy;
begin
  fScanEvent.Free;
  inherited Destroy;
end;

function TScanService.ScanDevice(pName: String): TDevice;
begin
  // switch to scan mode to handle raw message in special way
  fScanning := True;
  fScannedName:=pName;
  // send message to main form to switch GUI to scan mode
  if (Glb.MainFormHandle > 0) then
    PostMessage(Glb.MainFormHandle, WM_SCANNING_STATUS_CHANGE, 0, 0);
  // this is called from LUA execution thread
  // wait for signal from GUI thread that scanning is done
  fScanEvent.WaitFor(INFINITE);
  // now we should have something ready
  fScanEvent.ResetEvent;
  Result := fScannedDevice;
end;

end.

