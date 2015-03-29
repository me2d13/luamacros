unit uGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uXplControl, uLuaEngine, uDeviceService, uDevice, uHookService,
  uKeyLogService, windows;

type

  { TGlobals }

  TLogFunction = procedure(pMessage: String) of object;

  TGlobals = class
    private
      fXplCLcontrol: TXPLcontrol;
      fLuaEngine: TLuaEngine;
      fLogFunction: TLogFunction;
      fLoggedModules: TStrings;
      fLogAll: boolean;
      fDeviceService: TDeviceService;
      fHookService: THookService;
      fKeyLogService: TKeyLogService;
      fSpoolFileName: String;
      fLogCs: TRTLCriticalSection;
      fMainFormHandle: LongInt;
      fPrintBuffer: TStrings;
      procedure SetSpoolFileName(AValue: String);
      procedure AskMainFormToFlushPrintBuffer;
    public
      constructor Create;
      destructor Destroy; Override;
      procedure Init;
      procedure DebugLog(pMessage: String; pLogger: String);
      procedure DebugLogFile(pMessage: String; pLogger: String; pFileName: String);
      procedure DebugLogFmt(pMessage: String; const Args : Array of const; pLogger: String);
      procedure LogError(pMessage: String; pLogger: String);
      procedure LogModule(pLogger: String);
      procedure Print(pMessage: String);
      function IsModuleLogged(pLogger: String) : boolean;
      procedure TickMe;
      function ScanDevice: TDevice;
      procedure FlushBuffer;

      property XplControl: TXPLcontrol read fXplCLcontrol;
      property LogFunction: TLogFunction read fLogFunction write fLogFunction;
      property LogAll: Boolean read fLogAll write fLogAll;
      property LuaEngine:TLuaEngine read fLuaEngine;
      property DeviceService: TDeviceService read fDeviceService;
      property HookService: THookService read fHookService;
      property KeyLogService: TKeyLogService read fKeyLogService;
      property SpoolFileName: String read fSpoolFileName write SetSpoolFileName;
      property MainFormHandle: LongInt read fMainFormHandle write fMainFormHandle;
  end;

  LmcException = class (Exception)

  end;

var
  Glb : TGlobals;

const
  cUnassigned = '<unassigned>';
  WM_FLUSH_PRINT_BUFFER = WM_USER + 320;

implementation

{$Define offHARD_LOG}

const
  cLogFileName = 'LmcApp.log';

{ TGlobals }

procedure TGlobals.SetSpoolFileName(AValue: String);
begin
  if fSpoolFileName=AValue then Exit;
  fSpoolFileName:=AValue;
  if fSpoolFileName = '' then Exit; // spool cancelled
  if FileExists(fSpoolFileName) then
    SysUtils.DeleteFile(fSpoolFileName);
end;

procedure TGlobals.AskMainFormToFlushPrintBuffer;
begin
  if (fMainFormHandle > 0) then
    PostMessage(fMainFormHandle, WM_FLUSH_PRINT_BUFFER, 0, 0);
end;

constructor TGlobals.Create;
begin
  InitCriticalSection(fLogCs);
  fPrintBuffer := TStringList.Create;
  fXplCLcontrol:=TXPLcontrol.Create;
  fLoggedModules := TStringList.Create;
  fLuaEngine := TLuaEngine.Create;
  fDeviceService := TDeviceService.Create;
  fHookService := THookService.Create;
  fKeyLogService := TKeyLogService.Create;
end;

destructor TGlobals.Destroy;
begin
  fXplCLcontrol.Free;
  fLoggedModules.Free;
  fLuaEngine.Free;
  fDeviceService.Free;
  fHookService.Free;
  fKeyLogService.Free;
  fPrintBuffer.Free;
  DoneCriticalsection(fLogCs);
  inherited Destroy;
end;

procedure TGlobals.Init;
begin
  fXplCLcontrol.Init;
  fLuaEngine.Init;
  fDeviceService.Init;
end;

procedure TGlobals.DebugLog(pMessage: String; pLogger: String);
begin
  if IsModuleLogged(pLogger) then
  begin
    EnterCriticalSection(fLogCs);
    try
      if (fSpoolFileName > '') then
      begin
        DebugLogFile(pMessage, pLogger, fSpoolFileName);
      end else if Assigned(fLogFunction) then
        //fLogFunction(Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
        fPrintBuffer.Add(Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
    finally
      LeaveCriticalSection(fLogCs);
    end;
  end;
  AskMainFormToFlushPrintBuffer;
end;

procedure TGlobals.DebugLogFile(pMessage: String; pLogger: String;
  pFileName: String);
var
  lFile: TextFile;
begin
  if IsModuleLogged(pLogger) then
  begin
    AssignFile(lFile, pFileName);
    if FileExists(pFileName) then
      Append(lFile)
    else
      Rewrite(lFile);
    Write(lFile, Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
    WriteLn(lFile);
    CloseFile(lFile);
  end;
end;

procedure TGlobals.DebugLogFmt(pMessage: String; const Args: array of const;
  pLogger: String);
begin
  DebugLog(Format(pMessage, Args), pLogger);
end;

procedure TGlobals.LogError(pMessage: String; pLogger: String);
begin
  if Assigned(fLogFunction) then
  begin
    EnterCriticalSection(fLogCs);
    try
      //fLogFunction(Format('%s [%s] ERROR: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
      fPrintBuffer.Add(Format('%s [%s] ERROR: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
    finally
      LeaveCriticalSection(fLogCs);
    end;
  end;
  AskMainFormToFlushPrintBuffer;
end;

procedure TGlobals.LogModule(pLogger: String);
begin
  if (not IsModuleLogged(pLogger)) then
    fLoggedModules.Add(pLogger);
end;

procedure TGlobals.Print(pMessage: String);
begin
  if Assigned(fLogFunction) then
  begin
    EnterCriticalSection(fLogCs);
    try
      //fLogFunction(pMessage);
      fPrintBuffer.Add(pMessage);
    finally
      LeaveCriticalSection(fLogCs);
    end;
  end;
  AskMainFormToFlushPrintBuffer;
end;

function TGlobals.IsModuleLogged(pLogger: String): boolean;
begin
  Result := (fLogAll or (fLoggedModules.IndexOf(pLogger) >= 0));
end;

procedure TGlobals.TickMe;
begin
  fDeviceService.TickMe;;
end;

function TGlobals.ScanDevice: TDevice;
begin

end;

procedure TGlobals.FlushBuffer;
var
  lItem: String;
begin
  // THIS MUST BE CALLED ONLY FROM MAIN APP THREAD
  if Assigned(fLogFunction) then
  begin
    EnterCriticalSection(fLogCs);
    try
      for lItem in fPrintBuffer do
        fLogFunction(lItem);
      fPrintBuffer.Clear;
    finally
      LeaveCriticalSection(fLogCs);
    end;
  end;
end;

initialization
  Glb := TGlobals.Create;

finalization;
  Glb.Free;

end.

