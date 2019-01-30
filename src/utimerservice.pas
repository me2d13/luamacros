unit uTimerService;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl;

type

  { TWaitingThread }

  TWaitingThread = class (TThread)
  private
    fInterval: Integer;
    fHandlerRef: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; pInterval: Integer; pHandlerRef: Integer);
    destructor Destroy;override;
  end;

  TTimersList = TFPGObjectList<TWaitingThread>;



  { TTimerService }

  TTimerService = class
    private
      fTimers: TTimersList;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure AddTimer(pInterval: Integer; pHandlerRef: Integer);
  end;


implementation

uses
  uGlobals;

{ TWaitingThread }

procedure TWaitingThread.Execute;
var lNow: Int64;
begin
  Glb.DebugLog(Format('Sleep thread: going to sleep for %d ms',
        [fInterval]), cLoggerTmr);
  Sleep(fInterval);
  Glb.DebugLog(Format('Sleep thread: sleep for %d ms is done, calling lua',
        [fInterval]), cLoggerTmr);
  lNow:=GetTickCount64;
  Glb.LuaEngine.CallFunctionByRef(fHandlerRef, lNow);
end;

constructor TWaitingThread.Create(CreateSuspended: Boolean; pInterval: Integer;
  pHandlerRef: Integer);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  fInterval := pInterval;
  fHandlerRef:=pHandlerRef;
end;

destructor TWaitingThread.Destroy;
begin
  inherited Destroy;
end;

{ TTimerService }

constructor TTimerService.Create;
begin
  fTimers := TTimersList.Create();
end;

destructor TTimerService.Destroy;
begin
  fTimers.Free;
end;

procedure TTimerService.AddTimer(pInterval: Integer; pHandlerRef: Integer);
var lSleepThread: TWaitingThread;
begin
  Glb.DebugLog(Format('Creating new sleep thread with interval %d and handler %d',
        [pInterval, pHandlerRef]), cLoggerTmr);
  lSleepThread := TWaitingThread.Create(true, pInterval, pHandlerRef);
  lSleepThread.Start;
end;

end.

