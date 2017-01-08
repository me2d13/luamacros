unit uTiming;

{$mode delphi}

interface

uses
  Classes, SysUtils, epiktimer;

type
  { TStopWatch }

  TStopWatch = class
    private
      fTimer: TEpikTimer;
      fStartTicks: Int64;
    public
      procedure Start;
      function StopAndGetUsec: Int64;
      function StopAndLog: String;
      constructor Create;
      destructor Destroy; Override;
  end;


implementation

{ TStopWatch }

procedure TStopWatch.Start;
begin
  fStartTicks := fTimer.GetSystemTicks;
end;

function TStopWatch.StopAndGetUsec: Int64;
begin
  result := fTimer.GetSystemTicks - fStartTicks;
end;

function TStopWatch.StopAndLog: String;
begin
  result := Format('%s - %d', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), StopAndGetUsec]);
end;

constructor TStopWatch.Create;
begin
  fTimer := TEpikTimer.Create(nil);
end;

destructor TStopWatch.Destroy;
begin
  fTimer.Free;
  inherited Destroy;
end;

end.

