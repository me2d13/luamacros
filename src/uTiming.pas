unit uTiming;

{$mode delphi}

interface

uses
  Classes, SysUtils, epiktimer;

type
  { TStopWatch }

  TStopWatch = class
    private
      fStart: TEpikTimer;
      fStartTicks: Int64;
    public
      procedure Start;
      function StopAndGetUsec: Extended;
      function StopAndLog: String;
      constructor Create;
      destructor Destroy; Override;
  end;


implementation

{ TStopWatch }

procedure TStopWatch.Start;
begin
  fStartTicks := fStart.GetSystemTicks;
end;

function TStopWatch.StopAndGetUsec: Extended;
begin
  result := fStart.Elapsed;
end;

function TStopWatch.StopAndLog: String;
begin
  result := Format('%s - %d', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), fStart.GetSystemTicks - fStartTicks]);
end;

constructor TStopWatch.Create;
begin
  fStart := TEpikTimer.Create(nil);
end;

destructor TStopWatch.Destroy;
begin
  fStart.Free;
  inherited Destroy;
end;

end.

