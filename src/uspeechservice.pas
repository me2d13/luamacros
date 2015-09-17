unit uSpeechService;

{$mode delphi}

interface

uses
  Classes, SysUtils, comobj, syncobjs;

type

  { TSpeechWorker }

  TSpeechWorker = class (TThread)
    protected
      fRlSynchronizer: TMultiReadExclusiveWriteSynchronizer;
      fList: TStrings;
      fSpVoice: Variant;
      fEvent: TEventObject;
      procedure Execute; override;
      procedure DoSay(pWhat: Variant);
    public
      constructor Create;
      destructor Destroy;override;
      procedure Say(pWhat: String);
      procedure Stop;
  end;

  { TSpeechService }

  TSpeechService = class
    private
      fWorker: TSpeechWorker;
    public
      constructor Create;
      destructor Destroy;override;
      procedure Say(pWhat: String);
  end;

implementation

uses uGlobals, windows, ActiveX;

{ TSpeechWorker }

procedure TSpeechWorker.Execute;
var
  lSize: Integer;
  lSay: String;
begin
  CoInitialize(nil);
  try
    while (not Terminated) do
    begin
      fRlSynchronizer.Beginread;
      lSize := fList.Count;
      fRlSynchronizer.Endread;
      if (lSize = 0) then
      begin
        Glb.DebugLog('Speech worker: queue size is 0, suspending thread...', cLoggerSpe);
        fEvent.WaitFor(INFINITE);
        fEvent.ResetEvent;
        Glb.DebugLog('Speech worker: resumed...', cLoggerSpe);
      end
      else
      begin
        fRlSynchronizer.Beginwrite;
        try
          lSay := fList[0];
          fList.Delete(0);
        finally
          fRlSynchronizer.Endwrite;
        end;
        DoSay(lSay);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TSpeechWorker.DoSay(pWhat: Variant);
var
  SavedCW: Word;
begin
  // Change FPU interrupt mask to avoid SIGFPE exceptions
  SavedCW := Get8087CW;
  try
    //Set8087CW(SavedCW or $4);
    Set8087CW(SavedCW or $1 or $2 or $4 or $8 or $16 or $32);
    fSpVoice.Speak(pWhat, 0);
  finally
    // Restore FPU mask
    Set8087CW(SavedCW);
  end;
end;

constructor TSpeechWorker.Create;
begin
  inherited Create(True);
  fRlSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  fList := TStringList.Create;
  fSpVoice := CreateOleObject('SAPI.SpVoice');
  fEvent:=TEventObject.Create(nil, true, false, 'LmcSpeech');
end;

destructor TSpeechWorker.Destroy;
begin
  fList.Free;
  fRlSynchronizer.Free;
  //fEvent.Free; execute.reset can be called on freed object
  inherited Destroy;
end;

procedure TSpeechWorker.Say(pWhat: String);
begin
  fRlSynchronizer.Beginwrite;
  try
    fList.Add(pWhat);
  finally
    fRlSynchronizer.Endwrite;
  end;
  fEvent.SetEvent; // even if it runs already
end;

procedure TSpeechWorker.Stop;
begin
  Terminate;
  fEvent.SetEvent;
end;

{ TSpeechService }

constructor TSpeechService.Create;
begin
  fWorker := TSpeechWorker.Create;
  fWorker.Start;
end;

destructor TSpeechService.Destroy;
begin
  fWorker.Stop;
  fWorker.WaitFor;
  fWorker.Free;
  inherited Destroy;
end;

procedure TSpeechService.Say(pWhat: String);
begin
  fWorker.Say(pWhat);
end;

end.

