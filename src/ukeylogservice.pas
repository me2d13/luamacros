unit uKeyLogService;

{$mode delphi}

interface

uses
  uRawInput, Classes, SysUtils, uKbdDevice;

const
  cLogArrayLength = 100;
  cGarbageDelayMs = 60000; // 1 min

type
  TKeyLogItem = record
    TimeStamp: Int64;
    KeyStroke: TKeyStroke;
  end;

  { TKeyLogService }

  TKeyLogService = class
    private
      fLog: Array[0..cLogArrayLength] of TKeyLogItem;
      fIndex: Integer; // points to first free block
      function AssignDeviceInLogRange(var pKS: TKeyStroke; pSearchCount: Integer
        ): TKeyStrokePtr;
      function KeyStrokeEqual(p1, p2: TKeyStroke): boolean;
    public
      constructor Create;
      function AddRaw(pRawdata: PRAWINPUT): TKeyStrokePtr;
      function AssignDevice(var pKS: TKeyStroke): TKeyStrokePtr;
      function UnixTimestampMs: LongInt;
      procedure RemoveOldItems;
  end;

implementation

uses
  Windows, dateutils, uDevice, uGlobals, uHookService;

{ TKeyLogService }

function TKeyLogService.KeyStrokeEqual(p1, p2: TKeyStroke): boolean;
begin
  Result := (p1.VKeyCode = p2.VKeyCode) and (p1.Direction = p2.Direction);
end;

constructor TKeyLogService.Create;
begin
  fIndex:=0;
  ZeroMemory(@fLog[0], cLogArrayLength * SizeOf(TKeyLogItem));
end;

function TKeyLogService.AddRaw(pRawdata: PRAWINPUT): TKeyStrokePtr;
begin
  case pRawdata^.keyboard.Message of
    WM_KEYDOWN, WM_SYSKEYDOWN: fLog[fIndex].KeyStroke.Direction:=cDirectionDown;
    WM_KEYUP, WM_SYSKEYUP: fLog[fIndex].KeyStroke.Direction:=cDirectionUp;
  end;
  fLog[fIndex].TimeStamp:=UnixTimestampMs;
  fLog[fIndex].KeyStroke.DeviceHandle:=pRawdata^.header.hDevice;
  fLog[fIndex].KeyStroke.VKeyCode:=pRawdata^.keyboard.VKey;
  Result := @(fLog[fIndex].KeyStroke);
  fIndex:=(fIndex+1) mod cLogArrayLength;
end;

function TKeyLogService.AssignDevice(var pKS: TKeyStroke): TKeyStrokePtr;
var
  lNewItemsCount: Integer;
  lLogKeyStroke: TKeyStrokePtr;
begin
  // first search in log - already arrived raw messages
  lLogKeyStroke:=AssignDeviceInLogRange(pKS, cLogArrayLength);
  if (lLogKeyStroke <> nil) then
  begin
    Result:= lLogKeyStroke;
    exit;
  end;
  Glb.DebugLogFmt('Raw message not yet arrived for key %d direction %d, trying PeekMessage.',
    [pKS.VKeyCode,pKS.Direction], cHookLoggerName);
  // if not found, check main window message queue for incoming messages
  lNewItemsCount := Glb.DeviceService.KbdDeviceService.ProcessWaitingRawMessages;
  if (lNewItemsCount > 0) then
  begin
    Glb.DebugLogFmt('PeekMessage got %d messages.', [lNewItemsCount], cHookLoggerName);
    lLogKeyStroke:=AssignDeviceInLogRange(pKS, lNewItemsCount);
    if lLogKeyStroke <> nil then
    begin
      Result := lLogKeyStroke;
      exit;
    end;
  end;
  Glb.DebugLogFmt('Key NOT FOUND in key log for key %d direction %d.',
    [pKS.VKeyCode,pKS.Direction], cHookLoggerName);
  Result := nil;
end;

function TKeyLogService.AssignDeviceInLogRange(var pKS: TKeyStroke; pSearchCount: Integer): TKeyStrokePtr;
var
  I: Integer;
  lIndex: Integer;
  lTimeDiff: Integer;
  lNewItemsCount: Integer;
begin
  // heart of LuaMacros
  // search log of received low level messages (with specific keyboard id)
  // and match it to received key via hook message (from active window)
  // add keybaord id into the param
  Result := nil;
  for I := 1 to pSearchCount do
  begin
    lIndex:=(fIndex + cLogArrayLength - I) mod cLogArrayLength;
    if (fLog[lIndex].TimeStamp <> 0) then
    begin
      if (KeyStrokeEqual(fLog[lIndex].KeyStroke, pKS)) then
      begin
        lTimeDiff:=UnixTimestampMs - fLog[lIndex].TimeStamp;
        Glb.DebugLogFmt('Key log match for key %d direction %d, time diff %d ms.',
          [pKS.VKeyCode,pKS.Direction, lTimeDiff], cHookLoggerName);
        pKS.DeviceHandle:=fLog[lIndex].KeyStroke.DeviceHandle;
        // reset buffer pos
        ZeroMemory(@fLog[lIndex], SizeOf(TKeyLogItem));
        Result := @(fLog[lIndex].KeyStroke);
        break;
      end;
    end;
  end;
end;

function TKeyLogService.UnixTimestampMs: LongInt;
begin
  Result := Round(Now * 24*60*60*1000);
end;

procedure TKeyLogService.RemoveOldItems;
var
  I: Integer;
  lIndex: Integer;
  lKsPtr: TKeyStrokePtr;
  lFiredCnt: Integer;
  lNow: LongInt;
begin
  lNow := UnixTimestampMs;
  lFiredCnt := 0;
  for I := 1 to cLogArrayLength do
  begin
    lIndex:=(fIndex + cLogArrayLength - I) mod cLogArrayLength;
    lKsPtr := @(fLog[lIndex].KeyStroke);
    // clean garbage
    if (lNow - fLog[lIndex].TimeStamp > cGarbageDelayMs) then
    begin
      Glb.DebugLogFmt('WARNING: Removing unmatched raw message after %d ms. The key was %d, direction %d.',
        [lNow - fLog[lIndex].TimeStamp, lKsPtr^.VKeyCode, lKsPtr^.Direction], cHookLoggerName);
      // reset buffer pos
      ZeroMemory(@fLog[lIndex], SizeOf(TKeyLogItem));
    end;
  end;
end;

end.

