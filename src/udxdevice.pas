unit uDxDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice, DirectInput;

type

  { TDxDevice }

  TDxDevice = class(TDevice)
    private
      fButtonsCount: Integer;
      fDIDevice: IDIRECTINPUTDEVICE8;
      fJoyState: DIJOYSTATE2;

    public
      function TypeCaption: String; override;
      procedure DetectChanges;

      property ButtonsCount : Integer read fButtonsCount write fButtonsCount;
      property DIDevice : IDIRECTINPUTDEVICE8 read fDIDevice write fDIDevice;
  end;

const
  cDxLoggerName = 'DX';

implementation

uses
  Windows, uGlobals;

{ TDxDevice }

function TDxDevice.TypeCaption: String;
begin
  Result := 'game';
end;

procedure TDxDevice.DetectChanges;
var
  I: Integer;
  lOldState: DIJoyState2;
  hr: HRESULT;
begin
  if fDIDevice = nil then
    exit;

  lOldState := fJoyState;
  // Get Buttons
  hr := fDIDevice.Poll;
  if (FAILED(hr)) then
  begin
    hr := fDIDevice.Acquire;
    if FAILED(hr) then
    begin
      Glb.DebugLog('Warning: Can''t acquire joy ' + Name, cDxLoggerName);
      exit;
    end;
  end;
  hr := fDIDevice.GetDeviceState(SizeOf(DIJOYSTATE2), @fJoyState);
  if FAILED(hr) then
  begin
    Glb.DebugLog('Warning: Can''t acquire joy ' + Name, cDxLoggerName);
    exit;
  end;

  for I := 0 to fButtonsCount - 1 do
  begin
    if fJoyState.rgbButtons[I] <> lOldState.rgbButtons[I] then
    begin
      if lOldState.rgbButtons[I] = 0 then
        Glb.DebugLog(Format('Event: %s button %d DOWN', [LogIdent, I]), cDxLoggerName)
      else
        Glb.DebugLog(Format('Event: %s button %d UP', [LogIdent, I]), cDxLoggerName)
    end;
  end;
end;

end.

