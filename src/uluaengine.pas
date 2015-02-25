unit uLuaEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua;

type

  { TLuaEngine }

  TLuaEngine = class
    private
      fLua: TLua;
      fInitOk: boolean;
      procedure RegisterFunctions;
    public
      constructor Create;
      destructor Destroy;override;
      procedure runCode(pSource: String);
      procedure Init;
      procedure UnInit;
  end;

implementation

uses uMainFrm, uGlobals,
  uLuaCmdXpl, uLuaCmdDevice;

const
{$IFDEF UNIX}
{$IFDEF CPU64}
     LUALIBRARY = 'liblua52-64.so';
{$ELSE}
     LUALIBRARY = 'liblua52-32.so';
{$ENDIF}
{$ELSE}
{$IFDEF CPU64}
     LUALIBRARY = 'lua52-64.dll';
{$ELSE}
     LUALIBRARY = 'lua52-32.dll';
{$ENDIF}
{$ENDIF}

function FormPrint(luaState : TLuaState) : integer;
var arg : PAnsiChar;
begin
     arg := lua_tostring(luaState, 1);
     MainForm.print(arg);
     Lua_Pop(luaState, Lua_GetTop(luaState));
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
var arg : PAnsiChar;
begin
     Glb.LogAll:=true;
     Result := 0;
end;

{ TLuaEngine }

procedure TLuaEngine.RegisterFunctions;
begin
  if (fLua = nil) then
  begin
    Glb.LogError('Can''t register Lua functions, LUA init failed.', 'LUA');
    exit;
  end;
  fLua.RegisterFunction('lmc_xpl_command','lmc_xpl_command',nil,@XplCommand);
  fLua.RegisterFunction('print','lmc_print',nil,@FormPrint);
  fLua.RegisterFunction('lmc_log_module','lmc_log_module',nil,@LogModule);
  fLua.RegisterFunction('lmc_log_all','lmc_log_all',nil,@LogAll);
  fLua.RegisterFunction('lmc_print_devices','lmc_print_devices',nil,@PrintDevices);
  fLua.RegisterFunction('lmc_device_name_check_ask','lmc_device_name_check_ask',nil,@CheckDeviceNameWithAsk);
end;

constructor TLuaEngine.Create;
begin
  //Tries loading the dynamic library file
  fInitOk := true;
  if not Lua.libLoaded then
  begin
    if not Lua.LoadLuaLibrary(LUALIBRARY) then
    begin
      Glb.LogError('Lua library could not load : ' + Lua.errorString, 'LUA');
      fInitOk:=false;
    end;
  end;
  if (fInitOk) then begin
    fLua := TLua.Create();
  end
  else
  begin
    fLua := nil;
  end;
end;

destructor TLuaEngine.Destroy;
begin
  if Assigned(fLua) then
    fLua.Free;
  inherited Destroy;
end;

procedure TLuaEngine.runCode(pSource: String);
var
  lMes: String;
  lRes: Integer;
begin
  try
    lRes := luaL_loadbuffer(fLua.LuaInstance,PAnsiChar(pSource), Length(pSource), 'LuaMacros script');
    if lRes <> 0 then
      raise Exception.Create('Cannot load buffer.');

    lRes := lua_pcall(fLua.LuaInstance, 0, LUA_MULTRET, 0);
    case lRes of
      LUA_ERRRUN    : raise Exception.Create('Runtime error');
      LUA_ERRMEM    : raise Exception.Create('Memory allocation error');
      LUA_ERRSYNTAX : raise Exception.Create('Syntax error');
      LUA_ERRERR    : raise Exception.Create('Error handling function failed');
    end;
  except
    on E:Exception do
    begin
      lMes := E.Message;
      if Assigned(fLua) then
        lMes := lMes + #10 + #09 + lua_tostring(fLua.LuaInstance, -1);
      Glb.LogError(lMes, 'LUA');
    end;
  end;
  //  fLua.RunFromMem(PAnsiChar(pSource), Length(pSource), 'LuaMacros script');
end;

procedure TLuaEngine.Init;
begin
  //loads the standard Lua toolkits
  luaL_openlibs(fLua.LuaInstance);
  //activates the Garbage collector on the Lua side
  lua_gc(fLua.LuaInstance, LUA_GCRESTART, 0);
  RegisterFunctions;
end;

procedure TLuaEngine.UnInit;
begin
  if (Assigned(fLua)) then
  begin
    //signals the garbage collector to start cleaning
    lua_gc(fLua.LuaInstance, LUA_GCCOLLECT, 0);
    //unregisters the function we loaded into the interpreter
    fLua.UnregisterFunctions(nil);
  end;
end;

end.

