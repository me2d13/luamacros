unit uHookService;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, uHookCommon, MemMap;

type

  { THookService }

  THookService = class
    private
      fSharedMemory: TMemMap;
      fSMPtr: PMMFData;
      fHookSet: Boolean;
      procedure InitSharedMemory(pMainFormHandle: THandle);
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Init(pMainFormHandle: THandle);
      procedure OnHookMessage(var pMessage: TMessage);
  end;

const
  HookLib = 'WinHook.dll';
  cHookLoggerName = 'HOOK';


  function SetHook: Boolean; stdcall; external HookLib;
  function FreeHook: Boolean; stdcall; external HookLib;


implementation

uses
  uGlobals;

{ THookService }

procedure THookService.InitSharedMemory(pMainFormHandle: THandle);
begin
  fSMPtr := nil;
  try
    fSharedMemory := TMemMap.Create(MMFName, SizeOf(TMMFData));
    fSMPtr := fSharedMemory.Memory;
  except
    on EMemMapException do
    begin
      Glb.LogError('Can''t create shared memory.', cHookLoggerName);
      fSharedMemory := nil;
    end;
  end;
  if fSMPtr <> nil then
  begin
    fSMPtr^.MainWinHandle := pMainFormHandle;
    fSMPtr^.LmcPID := GetCurrentProcessId;
    fSMPtr^.Debug:=1; // for now
  end;
end;

constructor THookService.Create;
begin
  fHookSet:=False;
end;

destructor THookService.Destroy;
begin
  if (fHookSet) then
    FreeHook;
end;

procedure THookService.Init(pMainFormHandle: THandle);
begin
  InitSharedMemory(pMainFormHandle);
  if (not fHookSet) then
  begin
    fHookSet := LongBool(SetHook);
  end;
end;

procedure THookService.OnHookMessage(var pMessage: TMessage);
begin
  Glb.DebugLog('Hook message received in LmcApp', cHookLoggerName);
  pMessage.Result:=0; // do not block
  //pMessage.Result:=-1; // block
end;

end.

