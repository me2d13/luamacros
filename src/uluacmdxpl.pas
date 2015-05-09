unit uLuaCmdXpl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function XplCommand(luaState : TLuaState) : integer;
function XplCommandBegin(luaState : TLuaState) : integer;
function XplCommandEnd(luaState : TLuaState) : integer;
function GetXplVariable(luaState : TLuaState) : integer;
function SetXplVariable(luaState : TLuaState) : integer;
function XplDrawText(luaState : TLuaState) : integer;


implementation

uses
  uGlobals, uXplCommon, variants, uXplMessages;

function XplCommand(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     //reads the first parameter passed to Increment as an integer
     arg := lua_tostring(luaState, 1);

     //print
     Glb.XplControl.ExecuteCommand(arg);

     //clears current Lua stack
     Lua_Pop(luaState, Lua_GetTop(luaState));

     //Result : number of results to give back to Lua
     Result := 0;
end;

function XplCommandBegin(luaState: TLuaState): integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.XplControl.ExecuteCommand(arg, HDMC_COMMAND_BEGIN);
     Result := 0;
end;

function XplCommandEnd(luaState: TLuaState): integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.XplControl.ExecuteCommand(arg, HDMC_COMMAND_END);
     Result := 0;
end;

function GetXplVariable(luaState: TLuaState): integer;
var arg : PAnsiChar;
  lRes: Variant;
  lMessage: String;
begin
     arg := lua_tostring(luaState, 1);
     lRes := Glb.XplControl.GetXplVariable(arg);
     if (VarIsStr(lRes)) then
     begin
       lMessage:=lRes;
       lua_pushstring(luaState, PChar(lMessage));
     end
     else if (VarIsOrdinal(lRes)) then lua_pushinteger(luaState, lRes)
     else if (VarIsFloat(lRes)) then lua_pushnumber(luaState, lRes)
     else
       begin
         lMessage:='Unexpected type returned from XPL with value ' + lRes;
         lua_pushstring(luaState, PChar(lMessage))
       end;
     Result := 1;
end;

function SetXplVariable(luaState: TLuaState): integer;
var arg : PAnsiChar;
  lVal: TXplValue;
begin
  arg := lua_tostring(luaState, 1);
  if (lua_isstring(luaState, 2) <> 0) then lVal := TXplValue.Create(lua_tostring(luaState, 2))
  else if (lua_isnumber(luaState, 2) = 0) then lVal := TXplValue.Create(lua_tonumber(luaState, 2))
  else
  begin
    raise Exception.Create('Unexpected variable type');
  end;
  Glb.DebugLog('Setting variable ' + arg + ' to ' + lVal.ToString, cLoggerXpl);
  Glb.XplControl.SetXplVariable(arg, lVal);
  Result := 0;
end;

function XplDrawText(luaState: TLuaState): integer;
var
  lText : PAnsiChar;
  lNumOfParams: Integer;
  lPos: Single;
  lSec: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  lText := lua_tostring(luaState, 1);
  if (lNumOfParams > 1) then
    lPos := lua_tonumber(luaState, 2)
  else
    lPos := 0.3;
  if (lNumOfParams > 2) then
    lSec := lua_tointeger(luaState, 3)
  else
    lSec := 5;
  Glb.XplControl.DrawText(lText, lPos, lSec);
  Result := 0;
end;

end.

