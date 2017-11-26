unit uXplLogger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TXplLogger }

  TXplLogger = class (TObject)
    private
      fLogLevel: Integer;
      fDebugLogFileName: String;
      function GetLogging: Boolean;
    public
      constructor Create();
      procedure Log(Value: String);
      procedure LogFmt(pFormat:String; pArgs: array of const);
      property LogLevel: Integer read fLogLevel write fLogLevel;
      property Logging: Boolean read GetLogging;
      property DebugLogFileName: String read fDebugLogFileName write fDebugLogFileName;
  end;

const
  cLlNone = 0;
  cLlInfo = 1;
  cLlDebug = 2;
  cLlTrace = 3;

implementation

const
  cLogFileTriggerName = 'lmc_log_file_trigger.log';


{ TXplLogger }

function TXplLogger.GetLogging: Boolean;
begin
  Result := fLogLevel > cLlNone;
end;

constructor TXplLogger.Create;
begin
  if (FileExists(cLogFileTriggerName)) then
  begin
    fLogLevel:=cLlDebug;
    fDebugLogFileName:=cLogFileTriggerName;
    Log('LuaMacros Plugin started');
  end
  else
    fLogLevel := cLlNone;
end;

procedure TXplLogger.Log(Value: String);
var
  lVal: String;
  logFile: TextFile;
  lThread: TThreadID;
begin
  if fLogLevel = cLlNone then
    exit;
  lThread:=GetCurrentThreadId;
  lVal := Format('%s [XPLLUMplugin %d]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), lThread, Value]);
  // to file
  AssignFile(logFile, fDebugLogFileName);
  if FileExists(fDebugLogFileName) then
      Append(logFile)
    else
      Rewrite(logFile);
  WriteLn(logFile, lVal);
  CloseFile(logFile);
end;

procedure TXplLogger.LogFmt(pFormat: String; pArgs: array of const);
begin
  Log(Format(pFormat, pArgs));
end;

end.

