unit uLuaCmdMainWindow;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua, uGlobals, uMainFrm;

function FormPrint(luaState : TLuaState) : integer;
function FormClear(luaState : TLuaState) : integer;
function LogModule(luaState : TLuaState) : integer;
function LogAll(luaState : TLuaState) : integer;
function LogSpool(luaState : TLuaState) : integer;
function SendKeys(luaState : TLuaState) : integer;
function SendInput(luaState : TLuaState) : integer;
function Spawn(luaState : TLuaState) : integer;
function MinimizeMainWindow(luaState : TLuaState) : integer;
function LoadScript(luaState : TLuaState) : integer;
function Say(luaState : TLuaState) : integer;
function GetActiveWindowTitle(luaState : TLuaState) : integer;
function DoSleep(luaState : TLuaState) : integer;

implementation

uses
  uSendKeys, Process, Windows;

function FormPrint(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.print(arg); // glb version is thread safe
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function FormClear(luaState : TLuaState) : integer;
begin
     gMainForm.ClearLog;
     Result := 0;
end;

function LogModule(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.LogModule(arg);
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function LogAll(luaState : TLuaState) : integer;
begin
     Glb.LogAll:=true;
     Result := 0;
end;

function LogSpool(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     Glb.SpoolFileName := arg;
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
end;

function SendKeys(luaState : TLuaState) : integer;
var
  arg : PAnsiChar;
  lSndKey: TKeySequence;
begin
  arg := lua_tostring(luaState, 1);
  lSndKey := TKeySequence.Create;
  lSndKey.Sequence := arg;
  lSndKey.Resume;
  Lua_Pop(luaState, Lua_GetTop(luaState));
  Result := 0;
end;

function SendInput(luaState: TLuaState): integer;
var
  lArg1: Integer;
  lArg2: Integer;
  lArg3: Integer;
  lNumOfParams: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 3) then
    raise LmcException.Create('Wrong number of arguments. Provide 3 numbers.');
  lArg1 := lua_tointeger(luaState, 1);
  lArg2 := lua_tointeger(luaState, 2);
  lArg3 := lua_tointeger(luaState, 3);
  Glb.DebugLogFmt('Sending input vk:%d, scan:%d, flags:%d', [lArg1, lArg2, lArg3], cLoggerKbd);
  SendKeyboardInput(lArg1, lArg2, lArg3);
  Result := 0;
end;

function Spawn(luaState : TLuaState) : integer;
var
  arg : PAnsiChar;
  lProcess: TProcess;
  I: Integer;
  lNumOfParams: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  arg := lua_tostring(luaState, 1);
  lProcess := TProcess.Create(nil);
  try
    lProcess.InheritHandles := False;
    lProcess.Options := [];
    lProcess.ShowWindow := swoShow;

    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 0 to GetEnvironmentVariableCount - 1 do
      lProcess.Environment.Add(GetEnvironmentString(I));

    for I := 2 to lNumOfParams do
      if lua_isstring(luaState, I) = 1 then
        lProcess.Parameters.Add(lua_tostring(luaState, I));

    lProcess.Executable := arg;
    lProcess.Execute;
  finally
    lProcess.Free;
  end;
  Result := 0;
end;

function MinimizeMainWindow(luaState: TLuaState): integer;
begin
  SendMessage(Glb.MainFormHandle, WM_MAIN_WINDOW_COMMAND, MWC_MINIMIZE, 0);
end;

function LoadScript(luaState: TLuaState): integer;
var
  arg : PAnsiChar;
  lNumOfParams: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 1) then
    raise LmcException.Create('Wrong number of arguments. Provide script name.');
  arg := lua_tostring(luaState, 1);
  if (FileExists(arg)) then
  begin
    Glb.DebugLog('Loading scrpt ' + arg, cLoggerLua);
    Glb.LuaEngine.ScriptToRun:=arg;
  end
  else
    Glb.LogError('Cannot load scrpt ' + arg + '. File not found.', cLoggerLua);
  Result := 0;
end;

function Say(luaState: TLuaState): integer;
var
  arg : PAnsiChar;
  lNumOfParams: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 1) then
    raise LmcException.Create('Wrong number of arguments. Provide string.');
  arg := lua_tostring(luaState, 1);
  Glb.DebugLog('Saying ' + arg, cLoggerLua);
  Glb.SpeechService.Say(arg);
  Result := 0;
end;

function GetActiveWindowTitle(luaState: TLuaState): integer;
var
  lNumOfParams: Integer;
  lHandle: THandle;
  lLen: LongInt;
  lTitle: string;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams > 0) then
    raise LmcException.Create('Wrong number of parameters. No argument expected.');
  lHandle := GetForegroundWindow;
  if lHandle <> 0 then
  begin
    lLen := GetWindowTextLength(lHandle) + 1;
    SetLength(lTitle, lLen);
    GetWindowText(lHandle, PChar(lTitle), lLen);
    lTitle := TrimRight(lTitle);
  end
  else
    lTitle:= '';
  lua_pushstring(luaState, PChar(lTitle));
  Result := 1;
end;

function DoSleep(luaState: TLuaState): integer;
var
  lArg: Integer;
  lNumOfParams: Integer;
begin
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 1) then
    raise LmcException.Create('Wrong number of arguments. Provide number of ms.');
  lArg := lua_tointeger(luaState, 1);
  Glb.DebugLogFmt('Sleeping for %d msec.', [lArg], cLoggerLua);
  Sleep(lArg);
  Result := 0;
end;


end.

