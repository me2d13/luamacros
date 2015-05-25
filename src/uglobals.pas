unit uGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uXplControl, uLuaEngine, uDeviceService, uDevice, uHookService,
  uKeyLogService, windows, uScanService;

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
      fScanService: TScanService;
      fVersion: String;
      procedure SetSpoolFileName(AValue: String);
      procedure AskMainFormToFlushPrintBuffer;
      procedure SetVersion(AValue: String);
    public
      constructor Create;
      destructor Destroy; Override;
      procedure Init;
      procedure InitConfigValues;
      procedure DebugLog(pMessage: String; pLogger: String);
      procedure DebugLogFile(pMessage: String; pLogger: String; pFileName: String);
      procedure DebugLogFmt(pMessage: String; const Args : Array of const; pLogger: String);
      procedure LogError(pMessage: String; pLogger: String);
      procedure LogModule(pLogger: String);
      procedure Print(pMessage: String);
      function IsModuleLogged(pLogger: String) : boolean;
      procedure TickMe;
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
      property ScanService: TScanService read fScanService;
      property Version: String read fVersion write SetVersion;
  end;

  LmcException = class (Exception)

  end;

var
  Glb : TGlobals;

const
  cUnassigned = '<unassigned>';
  WM_FLUSH_PRINT_BUFFER = WM_USER + 320;
  WM_SCANNING_STATUS_CHANGE = WM_USER + 330;
  WM_MAIN_WINDOW_COMMAND = WM_USER + 340;

  MWC_MINIMIZE = 1;

  cLoggerXpl = 'XPL';
  cLoggerLua = 'LUA';


function Sto_GetFmtFileVersion(const FileName: String = ''; const Fmt: String = '%d.%d.%d.%d'): String;


implementation

{$Define offHARD_LOG}

const
  cLogFileName = 'LmcApp.log';

  function Sto_GetFmtFileVersion(const FileName: String = '';
    const Fmt: String = '%d.%d.%d.%d'): String;
  var
    sFileName: String;
    iBufferSize: DWORD;
    iDummy: DWORD;
    pBuffer: Pointer;
    pFileInfo: Pointer;
    iVer: array[1..4] of Word;
  begin
    // set default value
    Result := '';
    // get filename of exe/dll if no filename is specified
    sFileName := FileName;
    if (sFileName = '') then
    begin
      // prepare buffer for path and terminating #0
      SetLength(sFileName, MAX_PATH + 1);
      SetLength(sFileName,
        GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
    end;
    // get size of version info (0 if no version info exists)
    iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
    if (iBufferSize > 0) then
    begin
      GetMem(pBuffer, iBufferSize);
      try
      // get fixed file info (language independent)
      GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
      // read version blocks
      iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
      iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
      finally
        FreeMem(pBuffer);
      end;
      // format result string
      Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
    end;
  end;

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

procedure TGlobals.InitConfigValues;
begin
  fLuaEngine.SetConfigItem('minimizeToTray', false);
  fLuaEngine.SetConfigItem('version', fVersion);
end;

procedure TGlobals.SetVersion(AValue: String);
begin
  if fVersion=AValue then Exit;
  fVersion:=AValue;
  fLuaEngine.SetConfigItem('version', AValue);
end;

constructor TGlobals.Create;
begin
  InitCriticalSection(fLogCs);
  fPrintBuffer := TStringList.Create;
  fXplCLcontrol:=TXPLcontrol.Create;
  fLoggedModules := TStringList.Create;
  fLuaEngine := TLuaEngine.Create;
  fScanService := TScanService.Create;
  fDeviceService := TDeviceService.Create;
  fHookService := THookService.Create;
  fKeyLogService := TKeyLogService.Create;
end;

destructor TGlobals.Destroy;
begin
  fXplCLcontrol.Free;
  fLuaEngine.Free;
  fDeviceService.Free;
  fHookService.Free;
  fKeyLogService.Free;
  fPrintBuffer.Free;
  DoneCriticalsection(fLogCs);
  fScanService.Free;
  fLoggedModules.Free;
  inherited Destroy;
end;

procedure TGlobals.Init;
begin
  fXplCLcontrol.Init;
  fLuaEngine.Init;
  fDeviceService.Init;
  InitConfigValues;
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
  Result := fLogAll or ((fLoggedModules <> nil) and (fLoggedModules.IndexOf(pLogger) >= 0));
end;

procedure TGlobals.TickMe;
begin
  fDeviceService.TickMe;;
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

