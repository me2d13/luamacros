unit uGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uXplControl, uLuaEngine, uDeviceService, uDevice, uHookService;

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
    public
      constructor Create;
      destructor Destroy; Override;
      procedure Init;
      procedure DebugLog(pMessage: String; pLogger: String);
      procedure DebugLogFmt(pMessage: String; const Args : Array of const; pLogger: String);
      procedure LogError(pMessage: String; pLogger: String);
      procedure LogModule(pLogger: String);
      procedure Print(pMessage: String);
      function IsModuleLogged(pLogger: String) : boolean;
      procedure TickMe;
      function ScanDevice: TDevice;

      property XplControl: TXPLcontrol read fXplCLcontrol;
      property LogFunction: TLogFunction read fLogFunction write fLogFunction;
      property LogAll: Boolean read fLogAll write fLogAll;
      property LuaEngine:TLuaEngine read fLuaEngine;
      property DeviceService: TDeviceService read fDeviceService;
      property HookService: THookService read fHookService;
  end;

var
  Glb : TGlobals;

const
  cUnassigned = '<unassigned>';

implementation

{$Define HARD_LOG}

const
  cLogFileName = 'LmcApp.log';

{ TGlobals }

constructor TGlobals.Create;
begin
  fXplCLcontrol:=TXPLcontrol.Create;
  fLoggedModules := TStringList.Create;
  fLuaEngine := TLuaEngine.Create;
  fDeviceService := TDeviceService.Create;
  fHookService := THookService.Create;
end;

destructor TGlobals.Destroy;
begin
  fXplCLcontrol.Free;
  fLoggedModules.Free;
  fLuaEngine.Free;
  fDeviceService.Free;
  fHookService.Free;
  inherited Destroy;
end;

procedure TGlobals.Init;
begin
  fXplCLcontrol.Init;
  fLuaEngine.Init;
  fDeviceService.Init;
end;

procedure TGlobals.DebugLog(pMessage: String; pLogger: String);
var
  lFile: TextFile;
begin
  if Assigned(fLogFunction) and IsModuleLogged(pLogger) then
  begin
    fLogFunction(Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
  end;
  {$IfDef HARD_LOG}
    AssignFile(lFile, cLogFileName);
    if FileExists(cLogFileName) then
      Append(lFile)
    else
      Rewrite(lFile);
    Write(lFile, Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
    WriteLn(lFile);
    CloseFile(lFile);
  {$EndIf}
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
    fLogFunction(Format('%s [%s] ERROR: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
  end;
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
    fLogFunction(pMessage);
  end;
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

initialization
  Glb := TGlobals.Create;

finalization;
  Glb.Free;

end.

