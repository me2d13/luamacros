unit uDxDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice, DirectInput;

const
  cMaxAxisCount = 8;

type

  { TDxDevice }

  TDxDevice = class(TDevice)
    private
      fButtonsCount: Integer;
      fAxisCount: Integer;
      fPOVsCount: Integer;
      fDIDevice: IDIRECTINPUTDEVICE8;
      fBufferSize: Integer;
      fAxisValue: array[0..cMaxAxisCount] of DWORD;
      function GetAxisValue(I: Integer): DWORD;
      procedure HandleEvent(event: DIDEVICEOBJECTDATA);
      function getAxisIndex(pOffset: DWORD): Integer;
      function getPovIndex(pOffset: DWORD): Integer;
    public
      function TypeCaption: String; override;
      procedure ReadEventBuffer;
      constructor Create;
      function GetButtonValue(pIndex: Integer): Integer;
      property ButtonsCount : Integer read fButtonsCount write fButtonsCount;
      property AxisCount : Integer read fAxisCount write fAxisCount;
      property POVsCount : Integer read fPOVsCount write fPOVsCount;
      property BufferSize: Integer read fBufferSize;
      property DIDevice : IDIRECTINPUTDEVICE8 read fDIDevice write fDIDevice;
      property AxisValue[I:Integer]: DWORD read GetAxisValue;
  end;

implementation

uses
  Windows, uGlobals;

const
  cBufferSize = 1000;
  cPov0Offset = 32;

{ TDxDevice }

procedure TDxDevice.HandleEvent(event: DIDEVICEOBJECTDATA);
var
  lAxisIndex : Integer;
  lPovIndex : Integer;
begin
  if (event.dwOfs >= DIJOFS_BUTTON0) and (event.dwOfs <= DIJOFS_BUTTON31) then
  begin
    Glb.DebugLogFmt('Device %s button event, offset %d, data %d', [Name, event.dwOfs, event.dwData], cLoggerDx);
    if (event.dwData > 0) then
    begin
      Glb.LuaEngine.OnDeviceEvent(self, event.dwOfs - DIJOFS_BUTTON0, cDirectionDown, event.dwTimeStamp);
    end else begin
      Glb.LuaEngine.OnDeviceEvent(self, event.dwOfs - DIJOFS_BUTTON0, cDirectionUp, event.dwTimeStamp);
    end;
  end else begin
    lAxisIndex := getAxisIndex(event.dwOfs);
    lPovIndex := getPovIndex(event.dwOfs);
    if (lAxisIndex >= 0) then begin
      Glb.DeviceService.DxDeviceService.HandleAxisEvent(self, event, lAxisIndex);
      fAxisValue[lAxisIndex] := event.dwData;
      {
      Glb.DebugLogFmt('Device %s axis: %05d  %05d  %05d  %05d  %05d  %05d  %05d  %05d', [Name,
      fAxisValue[0],
      fAxisValue[1],
      fAxisValue[2],
      fAxisValue[3],
      fAxisValue[4],
      fAxisValue[5],
      fAxisValue[6],
      fAxisValue[7]
      ], cLoggerDx);}
    end else if (lPovIndex >= 0) then begin
      Glb.DebugLogFmt('Device %s POV event, offset %d, data %d', [Name, event.dwOfs, event.dwData], cLoggerDx);
      Glb.LuaEngine.OnDeviceEvent(self, 1000+lPovIndex, event.dwData, event.dwTimeStamp);
    end else begin
         Glb.DebugLogFmt('Device %s unknown event, offset %d, data %d', [Name, event.dwOfs, event.dwData], cLoggerDx);
    end;
  end;
end;

function TDxDevice.GetAxisValue(I: Integer): DWORD;
begin
  Result := fAxisValue[I];
end;

function TDxDevice.getAxisIndex(pOffset: DWORD): Integer;
begin
  if (pOffset >= 0) and (pOffset <= 28) and (pOffset mod 4 = 0) then
    result := pOffset div 4
  else
    result := -1;
end;

function TDxDevice.getPovIndex(pOffset: DWORD): Integer;
begin
  if (pOffset >= cPov0Offset) and (pOffset <= cPov0Offset+3*SizeOf(DWORD)) and (pOffset mod SizeOf(DWORD) = 0) then
    result := (pOffset - cPov0Offset) mod SizeOf(DWORD)
  else
    result := -1;
end;

function TDxDevice.TypeCaption: String;
begin
  Result := 'game';
end;

procedure TDxDevice.ReadEventBuffer;
var
  I: Integer;
  hr: HRESULT;
  events: array[0..cBufferSize] of DIDEVICEOBJECTDATA;
  dwItems: DWORD;
begin
  if fDIDevice = nil then
    exit;

  hr := fDIDevice.Poll;
  if (FAILED(hr)) then
  begin
    hr := fDIDevice.Acquire;
    if FAILED(hr) then
    begin
      Glb.DebugLog('Warning: Can''t acquire game device ' + Name, cLoggerDx);
      exit;
    end;
  end;
  dwItems:=cBufferSize;
  hr := fDIDevice.GetDeviceData(SizeOf(DIDEVICEOBJECTDATA), events, dwItems, 0);
  if (hr = DI_BUFFEROVERFLOW) then
  begin
    Glb.DebugLog('DX event buffer overflow for device ' + Name + '. Increase buffer size or decrease polling interval', cLoggerDx);
  end;
  if (hr = DI_BUFFEROVERFLOW) or (hr = DI_OK) then
  begin
    //if (Name = 'LB2') then
    //Glb.DebugLogFmt('Device %s read %d events', [Name, dwItems], cLoggerDx);
    for I := 0 to dwItems - 1 do
    begin
      HandleEvent(events[i]);
      //Glb.DebugLogFmt('Device %s event, offset %d, data %d, sequence %d ', [Name, events[i].dwOfs, events[i].dwData, events[i].dwSequence], cLoggerDx);
    end;
  end else begin
    Glb.DebugLog('Can''t read buffered events for device ' + Name, cLoggerDx);
    exit;
  end;
end;

constructor TDxDevice.Create;
var
  I : Integer;
begin
  fBufferSize:=cBufferSize;
end;

function TDxDevice.GetButtonValue(pIndex: Integer): Integer;
var
  lJoyState: DIJOYSTATE2;
  hr: HRESULT;
begin
  hr := fDIDevice.GetDeviceState(SizeOf(DIJOYSTATE2), @lJoyState);
  if FAILED(hr) then
  begin
    Glb.DebugLog('Can''t read current button state for device ' + Name, cLoggerDx);
  end else begin
      if (pIndex <= 0) or (pIndex > fButtonsCount) then begin
        Glb.DebugLogFmt('Device %s has %d buttons. Provide value between 1 and %d', [Name, fButtonsCount, fButtonsCount], cLoggerDx);
        Result := 0;
      end
      else
        Result := lJoyState.rgbButtons[pIndex-1];
  end;
end;

end.

