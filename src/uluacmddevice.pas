unit uLuaCmdDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function PrintDevices(luaState : TLuaState) : integer;

implementation

uses
  uGlobals;

function PrintDevices(luaState : TLuaState) : integer;
begin
     //print
     Glb.DeviceService.ListDevices;
     //Result : number of results to give back to Lua
     Result := 0;
end;

function CheckDeviceNameWithAsk(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     //reads the first parameter passed to Increment as an integer
     arg := lua_tostring(luaState, 1);

     //print
     Glb.DeviceService.CheckNameAsk(arg);

     //clears current Lua stack
     Lua_Pop(luaState, Lua_GetTop(luaState));

     //Result : number of results to give back to Lua
     Result := 0;
end;


end.

