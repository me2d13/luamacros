unit uLuaCmdFs;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function FsEvent(luaState : TLuaState) : integer;

implementation

uses
  uGlobals;

function FsEvent(luaState: TLuaState): integer;
var name : PAnsiChar;
  param: Cardinal;
  lStart: Int64;
  lNumOfParams: Integer;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_fs_event');
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams < 1) then
    raise LmcException.Create('Wrong number of parameters. Provide event name and optional parameter.');
  name := lua_tostring(luaState, 1);
  if (lNumOfParams = 2) then
  begin
    if (lua_isnumber(luaState, 2) <> 0) then param := trunc(lua_tonumber(luaState, 2))
    else
      raise Exception.Create('Unexpected variable type, only number is supported as event argument');
  end else begin
    param := 0;
  end;

  Glb.FsService.SendEvent(name, param);

  //Result : number of results to give back to Lua
  Result := 0;
  Glb.StatsService.EndCommand('lmc_fs_event', lStart);
end;

end.

