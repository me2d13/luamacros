unit uLuaCmdHttp;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function HttpServerSimple(luaState : TLuaState) : integer;
function HttpGet(luaState : TLuaState) : integer;


implementation

uses
  uGlobals;

function HttpServerSimple(luaState: TLuaState): integer;
var
  lPort : Integer;
  lHandlerRef: Integer;
  lNumOfParams: Integer;
begin
  //Glb.LuaEngine.StackDump(luaState);
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams < 2) then
    raise LmcException.Create('Wrong number of parameters. Provide at least port and handler.');
  if (lNumOfParams = 2) then
  begin
    if (lua_isnumber(luaState, 1) = 1) then
    begin
      lPort:=Trunc(lua_tonumber(luaState, 1));
      //lua_settop(luaState, 1);
    end
    else
      raise LmcException.Create('1st parameter is supposed to be a number.');
  end
  else
    lPort:=80;
  lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
  Glb.DebugLog(Format('Got function reference with key %d', [lHandlerRef]), cLoggerLua);
  Glb.HttpService.StartServer(lPort, lHandlerRef);
  Result := 0;
end;

function HttpGet(luaState: TLuaState): integer;
var
  lNumOfParams: Integer;
  lTimeOut: Integer;
  lUrl, lResult: String;
begin
  //Glb.LuaEngine.StackDump(luaState);
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams < 1) then
    raise LmcException.Create('Wrong number of parameters. Provide at URL address.');
  if (lNumOfParams = 2) then
  begin
    if (lua_isnumber(luaState, 2) = 1) then
    begin
      lTimeOut:=Trunc(lua_tonumber(luaState, 2));
      //lua_settop(luaState, 1);
    end
    else
      raise LmcException.Create('2nd parameter is supposed to be a number.');
  end
  else
    lTimeOut:=-1;
  lUrl := lua_tostring(luaState, 1);
  Glb.DebugLog(Format('Calling http get for url %s with timeout %d', [lUrl, lTimeOut]), cLoggerLua);
  lResult := Glb.HttpService.HttpGet(lUrl, lTimeOut);
  lua_pushstring(luaState, PChar(lResult));
  Result := 1;
end;

end.

