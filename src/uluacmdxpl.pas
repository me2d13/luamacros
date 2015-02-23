unit uLuaCmdXpl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function XplCommand(luaState : TLuaState) : integer;

implementation

uses
  uGlobals;

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

end.

