unit SimpleIPCWrapper;

// SimpleIPC has a flaw under unix (bug 17248).
// Use this workaround since there's no way to extend SimpleIPC classes directly (bug 19136)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC;

procedure InitServer(Server: TSimpleIPCServer);

function IsServerRunning(Client: TSimpleIPCClient): Boolean;

implementation

{$ifdef unix}
uses
  BaseUnix;

const
  //F_RDLCK = 0;
  F_WRLCK = 1;
  //F_UNLCK = 2;

function GetPipeFileName(Server: TSimpleIPCServer): String;
begin
  Result := Server.ServerID;
  if not Server.Global then
    Result := Result + '-' + IntToStr(fpGetPID);
  Result := '/tmp/' + Result;
end;

function GetPipeFileName(Client: TSimpleIPCClient): String;
begin
  Result := Client.ServerID;
  if Client.ServerInstance <> '' then
    Result := Result + '-' + Client.ServerInstance;
  Result := '/tmp/' + Result;
end;

function SetLock(FileDescriptor: cint): Boolean;
var
  LockInfo: FLock;
begin
  LockInfo.l_type := F_WRLCK;
  LockInfo.l_whence := SEEK_SET;
  LockInfo.l_len := 0;
  LockInfo.l_start := 0;
  Result := FpFcntl(FileDescriptor, F_SetLk, LockInfo) <> -1;
end;

procedure InitServer(Server: TSimpleIPCServer);
var
  PipeFileName: String;
  PipeDescriptor: cint;
begin
  Server.StartServer;
  PipeFileName := GetPipeFileName(Server);
  PipeDescriptor := FpOpen(PipeFileName, O_RDWR, $1B6);
  if PipeDescriptor <> -1 then
  begin
    //Pipe file created. Try to set the lock
    if not SetLock(PipeDescriptor) then
    begin
      FpClose(PipeDescriptor);
      raise Exception.CreateFmt('UniqueInstance - A server instance of %s is already running', [Server.ServerID]);
    end;
  end
  else
    raise Exception.CreateFmt('UniqueInstance - Error creating pipe file for server %s', [Server.ServerID]);
end;

function IsServerRunning(Client: TSimpleIPCClient): Boolean;
var
  PipeFileName: String;
  PipeDescriptor: cint;
begin
  //check the pipe file
  PipeFileName := GetPipeFileName(Client);
  PipeDescriptor := FpOpen(PipeFileName, O_RDWR, $1B6);
  Result := PipeDescriptor <> -1;
  if Result then
  begin
    // pipe file exists
    // try to set the lock
    // if lock is created then is a stale file (server process crashed or killed)
    Result := not SetLock(PipeDescriptor);
    FpClose(PipeDescriptor);
    if not Result then
    begin
      //delete stale file
      FpUnlink(PipeFileName);
    end;
  end;
end;

{$else}

procedure InitServer(Server: TSimpleIPCServer);
begin
  Server.StartServer;
end;

function IsServerRunning(Client: TSimpleIPCClient): Boolean;
begin
  Result := Client.ServerRunning;
end;

{$endif}


end.

