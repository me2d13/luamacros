unit uGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uXplControl, uLuaEngine;

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
    public
      constructor Create;
      destructor Destroy; Override;
      procedure Init;
      procedure DebugLog(pMessage: String; pLogger: String);
      procedure LogError(pMessage: String; pLogger: String);
      function IsModuleLogged(pLogger: String) : boolean;
      property XplControl: TXPLcontrol read fXplCLcontrol;
      property LogFunction: TLogFunction read fLogFunction write fLogFunction;
      property LogAll: Boolean read fLogAll write fLogAll;
      property LuaEngine:TLuaEngine read fLuaEngine;
  end;

var
  Glb : TGlobals;

implementation

{ TGlobals }

constructor TGlobals.Create;
begin
  fXplCLcontrol:=TXPLcontrol.Create;
  fLoggedModules := TStringList.Create;
  fLuaEngine := TLuaEngine.Create;
end;

destructor TGlobals.Destroy;
begin
  fXplCLcontrol.Free;
  fLoggedModules.Free;
  fLuaEngine.Free;
  inherited Destroy;
end;

procedure TGlobals.Init;
begin
  fXplCLcontrol.Init;
  fLuaEngine.Init;
end;

procedure TGlobals.DebugLog(pMessage: String; pLogger: String);
begin
  if Assigned(fLogFunction) and (fLogAll or (fLoggedModules.IndexOf(pLogger) >= 0))then
  begin
    fLogFunction(Format('%s [%s]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
  end;
end;

procedure TGlobals.LogError(pMessage: String; pLogger: String);
begin
  if Assigned(fLogFunction) then
  begin
    fLogFunction(Format('%s [%s] ERROR: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), pLogger, pMessage]));
  end;
end;

function TGlobals.IsModuleLogged(pLogger: String): boolean;
begin

end;

initialization
  Glb := TGlobals.Create;

finalization;
  Glb.Free;

end.

