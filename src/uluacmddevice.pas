unit uLuaCmdDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua;

function PrintDevices(luaState : TLuaState) : integer;
function CheckDeviceNameWithAsk(luaState : TLuaState) : integer;
function AssignDeviceNameByRegexp(luaState : TLuaState) : integer;
function LuaCmdSetCallback(luaState : TLuaState) : integer;
function SetDeviceCallback(luaState : TLuaState) : integer;
function AddCom(luaState : TLuaState) : integer;

implementation

uses
  uGlobals, uDevice;

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

function AssignDeviceNameByRegexp(luaState: TLuaState): integer;
var
  lName : PAnsiChar;
  lRegexp : PAnsiChar;
  lResult : String;
begin
  lName := lua_tostring(luaState, 1);
  lRegExp := lua_tostring(luaState, 2);
  lResult := Glb.DeviceService.AssignNameByRegexp(lName, lRegexp);
  lua_pushstring(luaState, PChar(lResult));
  Result := 1;
end;

function LuaCmdSetCallback(luaState: TLuaState): integer;
var
  lDeviceName : PAnsiChar;
  lButton : Integer;
  lDirection : Integer;
  lHandlerRef: Integer;
  lNumOfParams: Integer;
begin
  // Device name
  // Button number
  // 0 = down, 1 = up
  // handler
  lNumOfParams:=lua_gettop(luaState);
  lDeviceName := lua_tostring(luaState, 1);
  if (lNumOfParams = 4) then
  begin
    lButton:= Trunc(lua_tonumber(luaState, 2));
    lDirection:= Trunc(lua_tonumber(luaState, 3));
    if (lDirection <> cDirectionUp) then
      lDirection:=cDirectionDown;
    lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
    Glb.LuaEngine.SetCallback(lDeviceName,lButton, lDirection, lHandlerRef);
  end;
  if (lNumOfParams = 2) then
  begin
    // whole device
    lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
    Glb.LuaEngine.SetDeviceCallback(lDeviceName, lHandlerRef);
  end;
  Result := 0;
end;

function SetDeviceCallback(luaState: TLuaState): integer;
begin

end;

function AddCom(luaState: TLuaState): integer;
var
  lDevName : PAnsiChar;
  lComName : PAnsiChar;
begin
  lDevName := lua_tostring(luaState, 1);
  lComName := lua_tostring(luaState, 2);
  Glb.DeviceService.AddCom(lDevName, lComName);
  Result := 0;
end;


end.

