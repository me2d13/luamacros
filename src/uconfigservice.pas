unit uConfigService;

{$mode delphi}

interface

uses
  Classes, Lua, SysUtils;

function SetParam(L: Lua_State): Integer; cdecl;
function GetParam(L: Lua_State): Integer; cdecl;

const
  cConfigVariableName = 'lmc';

  cParamMinimizeToTray = 'MINIMIZETOTRAY';
  cParamVersion = 'VERSION';
  cParamDxTimerIntervalMs = 'DXTIMERINTERVALMS';
  cParamAutoReload = 'AUTORELOAD';

type

  TInternalParamDescriptor = record
    Name: String;
    ParamType: Integer;
    ReadOnly: Boolean;
    Value: Pointer;
  end;

  TParamDescriptor = record
    ParamType: Integer;
    ReadOnly: Boolean;
  end;

  { TConfigService }

  TConfigService = class
    private
      fParams: array[0..3] of TInternalParamDescriptor;
      fMinimizeToTray: boolean;
      fAutoReload: boolean;
      fVersion: String;
      fDxTimerIntervalMs: Integer;
    public
      constructor Create;
      procedure InitParams;
      function GetDescriptor(pName: String): TParamDescriptor;
      function GetBoolean(pName: String): boolean;
      function GetString(pName: String): String;
      function GetInteger(pName: String): Integer;
      procedure SetBoolean(pName: String; pValue: Boolean);
      procedure SetVersion(pVersion: String);
      procedure SetInteger(pName: String; pValue: Integer);
  end;

implementation

uses
  uGlobals;

function SetParam(L: Lua_State): Integer; cdecl;
var
  lNumOfParams: Integer;
  arg : PAnsiChar;
  lBoolean: Boolean;
  lInteger: Integer;
  lDesc: TParamDescriptor;
begin
  lNumOfParams:=lua_gettop(L);
  if (lNumOfParams < 3) then
    raise LmcException.Create('Wrong number of parameters. Meta method should always provide name.');
  arg := lua_tostring(L, 2);
  lDesc:=Glb.ConfigService.GetDescriptor(arg);
  if (lDesc.ReadOnly) then
    raise LmcException.Create('Parameter ' + arg + ' is not writable.');
  if (lDesc.ParamType = vtBoolean) then
  begin
    if (lua_isboolean(L, 3)) then
    begin
      lBoolean := lua_toboolean(L, 3) <> 0;
      Glb.ConfigService.SetBoolean(arg, lBoolean);
    end
    else
      raise LmcException.Create('Parameter ' + arg + ' is supposed to be a boolean.');
  end else if (lDesc.ParamType = vtInteger) then
  begin
    if (lua_isnumber(L, 3) <> 0) then
    begin
      lInteger := lua_tointeger(L, 3);
      Glb.ConfigService.SetInteger(arg, lInteger);
    end
    else
      raise LmcException.Create('Parameter ' + arg + ' is supposed to be an integer.');
  end
  else
    raise LmcException.Create('Unsupported type of parameter ' + arg);
end;

function GetParam(L: Lua_State): Integer; cdecl;
var
  lNumOfParams: Integer;
  arg : PAnsiChar;
  lBoolean: Boolean;
  lDesc: TParamDescriptor;
  lInt: LongInt;
begin
  lNumOfParams:=lua_gettop(L);
  if (lNumOfParams < 2) then
    raise LmcException.Create('Wrong number of parameters. Meta method should always provide name.');
  arg := lua_tostring(L, 2);
  lDesc:=Glb.ConfigService.GetDescriptor(arg);
  if (lDesc.ParamType = vtBoolean) then
  begin
    if Glb.ConfigService.GetBoolean(arg) then
      lInt := 1
    else
      lInt := 0;
    lua_pushboolean(L, lInt);
  end else if (lDesc.ParamType = vtString) then
  begin
    lua_pushstring(L, PChar(Glb.ConfigService.GetString(arg)));
  end else if (lDesc.ParamType = vtInteger) then
  begin
    lua_pushinteger(L, Glb.ConfigService.GetInteger(arg));
  end
  else
    raise LmcException.Create('Unsupported type of parameter ' + arg);
  Result := 1;
end;

{ TConfigService }

constructor TConfigService.Create;
begin
  fMinimizeToTray:=False;
  fAutoReload:=False;
  fVersion:='?';
  fDxTimerIntervalMs:=50;
  InitParams;
end;

procedure TConfigService.InitParams;
begin
  fParams[0].Name:=cParamMinimizeToTray;
  fParams[0].ParamType:=vtBoolean;
  fParams[0].ReadOnly:=False;
  fParams[0].Value:=@fMinimizeToTray;

  fParams[1].Name:=cParamVersion;
  fParams[1].ParamType:=vtString;
  fParams[1].ReadOnly:=True;
  fParams[1].Value:=@fVersion;

  fParams[2].Name:=cParamDxTimerIntervalMs;
  fParams[2].ParamType:=vtInteger;
  fParams[2].ReadOnly:=False;
  fParams[2].Value:=@fDxTimerIntervalMs;

  fParams[3].Name:=cParamAutoReload;
  fParams[3].ParamType:=vtBoolean;
  fParams[3].ReadOnly:=False;
  fParams[3].Value:=@fAutoReload;
end;

function TConfigService.GetDescriptor(pName: String): TParamDescriptor;
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  //Glb.DebugLog('Getting desc ' + pName, cLoggerCfg);
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      Result.ParamType:=lPar.ParamType;
      Result.ReadOnly:=lPar.ReadOnly;
      exit;
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

function TConfigService.GetBoolean(pName: String): boolean;
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  Glb.DebugLog('Getting boolean ' + pName, cLoggerCfg);
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      if (lPar.ParamType = vtBoolean) then
      begin
        Result := PBoolean(lPar.Value)^;
        exit;
      end else
        raise LmcException.Create('Parameter ' + pName + ' is not a boolean.');
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

function TConfigService.GetString(pName: String): String;
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      if (lPar.ParamType = vtString) then
      begin
        Result := PString(lPar.Value)^;
        exit;
      end else
        raise LmcException.Create('Parameter ' + pName + ' is not a string.');
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

function TConfigService.GetInteger(pName: String): Integer;
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      if (lPar.ParamType = vtInteger) then
      begin
        Result := PInteger(lPar.Value)^;
        exit;
      end else
        raise LmcException.Create('Parameter ' + pName + ' is not an integer.');
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

procedure TConfigService.SetBoolean(pName: String; pValue: Boolean);
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  Glb.DebugLog('Setting boolean ' + pName, cLoggerCfg);
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      if (lPar.ParamType = vtBoolean) then
      begin
        PBoolean(lPar.Value)^ := pValue;
        exit;
      end else
        raise LmcException.Create('Parameter ' + pName + ' is not a boolean.');
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

procedure TConfigService.SetVersion(pVersion: String);
begin
  fVersion:=pVersion;
end;

procedure TConfigService.SetInteger(pName: String; pValue: Integer);
var
  lNameUpper: String;
  lPar: TInternalParamDescriptor;
begin
  Glb.DebugLog('Setting integer ' + pName, cLoggerCfg);
  lNameUpper:=UpperCase(pName);
  for lPar in fParams do
  begin
    if (lPar.Name = lNameUpper) then
    begin
      if (lPar.ParamType = vtInteger) then
      begin
        PInteger(lPar.Value)^ := pValue;
        exit;
      end else
        raise LmcException.Create('Parameter ' + pName + ' is not an integer.');
    end;
  end;
  raise LmcException.Create('Unknown parameter ' + pName);
end;

end.

