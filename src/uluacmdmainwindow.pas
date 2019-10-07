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
function SendUnicode(luaState : TLuaState) : integer;
function Spawn(luaState : TLuaState) : integer;
function MinimizeMainWindow(luaState : TLuaState) : integer;
function LoadScript(luaState : TLuaState) : integer;
function Say(luaState : TLuaState) : integer;
function GetActiveWindowTitle(luaState : TLuaState) : integer;
function DoSleep(luaState : TLuaState) : integer;
function DoReset(luaState : TLuaState) : integer;
function DoSetTimer(luaState : TLuaState) : integer;

implementation

uses
  uSendKeys, Process, Windows;

function FormPrint(luaState : TLuaState) : integer;
var arg : PAnsiChar;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('print');
     arg := lua_tostring(luaState, 1);
     Glb.print(arg); // glb version is thread safe
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
  Glb.StatsService.EndCommand('print', lStart);
end;

function FormClear(luaState : TLuaState) : integer;
var lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('clear');
  gMainForm.ClearLog;
  Result := 0;
  Glb.StatsService.EndCommand('clear', lStart);
end;

function LogModule(luaState : TLuaState) : integer;
var arg : PAnsiChar;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_log_module');
     arg := lua_tostring(luaState, 1);
     Glb.LogModule(arg);
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
     Glb.StatsService.EndCommand('lmc_log_module', lStart);
end;

function LogAll(luaState : TLuaState) : integer;
var lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_log_all');
     Glb.LogAll:=true;
     Result := 0;
     Glb.StatsService.EndCommand('lmc_log_all', lStart);
end;

function LogSpool(luaState : TLuaState) : integer;
var arg : PAnsiChar;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_log_spool');
     arg := lua_tostring(luaState, 1);
     Glb.SpoolFileName := arg;
     Lua_Pop(luaState, Lua_GetTop(luaState));
     Result := 0;
     Glb.StatsService.EndCommand('lmc_log_spool', lStart);
end;

function SendKeys(luaState : TLuaState) : integer;
var
  arg : PAnsiChar;
  arg2 : Integer;
  lSndKey: TKeySequence;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_send_keys');
  arg := lua_tostring(luaState, 1);
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams > 1) then
  begin
     arg2 := lua_tointeger(luaState, 2);
  end
  else
      arg2 := 0;
  lSndKey := TKeySequence.Create;
  lSndKey.Sequence := arg;
  lSndKey.DelayKeys:= arg2;
  lSndKey.Resume;
  Lua_Pop(luaState, Lua_GetTop(luaState));
  Result := 0;
  Glb.StatsService.EndCommand('lmc_send_keys', lStart);
end;

function SendInput(luaState: TLuaState): integer;
var
  lArg1: Integer;
  lArg2: Integer;
  lArg3: Integer;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_send_input');
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 3) then
    raise LmcException.Create('Wrong number of arguments. Provide 3 numbers.');
  lArg1 := lua_tointeger(luaState, 1);
  lArg2 := lua_tointeger(luaState, 2);
  lArg3 := lua_tointeger(luaState, 3);
  Glb.DebugLogFmt('Sending input vk:%d, scan:%d, flags:%d', [lArg1, lArg2, lArg3], cLoggerKbd);
  SendKeyboardInput(lArg1, lArg2, lArg3);
  Result := 0;
  Glb.StatsService.EndCommand('lmc_send_input', lStart);
end;

function SendUnicode(luaState: TLuaState): integer;
var
  lArg1: Integer;
  lArg2: Integer;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_send_unicode');
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 2) then
    raise LmcException.Create('Wrong number of arguments. Provide 2 numbers.');
  lArg1 := lua_tointeger(luaState, 1);
  lArg2 := lua_tointeger(luaState, 2);
  Glb.DebugLogFmt('Sending unicode input char code:%d, direction:%d', [lArg1, lArg2], cLoggerKbd);
  SendUnicodeInput(lArg1, lArg2);
  Result := 0;
  Glb.StatsService.EndCommand('lmc_send_unicode', lStart);
end;

function Spawn(luaState : TLuaState) : integer;
var
  arg : PAnsiChar;
  lProcess: TProcess;
  I: Integer;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_spawn');
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
  Glb.StatsService.EndCommand('lmc_spawn', lStart);
end;

function MinimizeMainWindow(luaState: TLuaState): integer;
var lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_minimize');
  SendMessage(Glb.MainFormHandle, WM_MAIN_WINDOW_COMMAND, MWC_MINIMIZE, 0);
  Glb.StatsService.EndCommand('lmc_minimize', lStart);
end;

function LoadScript(luaState: TLuaState): integer;
var
  arg : PAnsiChar;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_load');
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
  Glb.StatsService.EndCommand('lmc_load', lStart);
end;

function Say(luaState: TLuaState): integer;
var
  arg : PAnsiChar;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_say');
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 1) then
    raise LmcException.Create('Wrong number of arguments. Provide string.');
  arg := lua_tostring(luaState, 1);
  Glb.DebugLog('Saying ' + arg, cLoggerLua);
  Glb.SpeechService.Say(arg);
  Result := 0;
  Glb.StatsService.EndCommand('lmc_say', lStart);
end;

function GetActiveWindowTitle(luaState: TLuaState): integer;
var
  lNumOfParams: Integer;
  lHandle: THandle;
  lLen: LongInt;
  lTitle: string;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_get_window_title');
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
  Glb.StatsService.EndCommand('lmc_get_window_title', lStart);
end;

function DoSleep(luaState: TLuaState): integer;
var
  lArg: Integer;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_sleep');
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams <> 1) then
    raise LmcException.Create('Wrong number of arguments. Provide number of ms.');
  lArg := lua_tointeger(luaState, 1);
  Glb.DebugLogFmt('Sleeping for %d msec.', [lArg], cLoggerLua);
  Sleep(lArg);
  Result := 0;
  Glb.StatsService.EndCommand('lmc_sleep', lStart);
end;

function DoReset(luaState: TLuaState): integer;
var lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_reset');
  //Glb.Reset;  can't reset LUA interpreter in the middle of execution, just set the flag
  gMainForm.ResetAfterLuaScriptEnd;
  Result := 0;
  Glb.StatsService.EndCommand('lmc_reset', lStart);
end;

function DoSetTimer(luaState: TLuaState): integer;
var
  lHandlerRef: Integer;
  lInterval : Integer;
  lNumOfParams: Integer;
  lStart: Int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_set_timer');
  // Interval
  // handler
  lNumOfParams:=lua_gettop(luaState);
  if (lNumOfParams = 2) then
  begin
    if lua_isnumber(luaState, 1) = 1 then
      lInterval:= Trunc(lua_tonumber(luaState, 1))
    else
      raise LmcException.Create('Wrong type of 1st parameter. It must number [miliseconds]');
    lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
    Glb.TimerService.AddTimer(lInterval, lHandlerRef);
  end else
    raise LmcException.Create('Provide 2 parameters for lmc_set_timer - interval and callback function');
  Result := 0;
  Glb.StatsService.EndCommand('lmc_set_timer', lStart);
end;


end.

