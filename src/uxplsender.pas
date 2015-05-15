unit uXplSender;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleIpc, uXplMessages;

type

  TDebugMethod = procedure (pFormat:String; pArgs: array of const) of object;

  { TXplSender }

  TXplSender = class
    private
      fClient: TSimpleIPCClient;
      fDebugMethod: TDebugMethod;
      procedure DebugFmt(pFormat:String; pArgs: array of const);
      procedure DumpStream(pStream: TStream);
      function GetServerRunning: Boolean;
    public
      constructor Create(pName: String);
      destructor Destroy; Override;
      procedure SendString(pValue: String);
      procedure SendMessage(pMessage: TXplMessage);
      procedure Reconnect;
      property DebugMethod: TDebugMethod read fDebugMethod write fDebugMethod;
      property ServerRunning: Boolean read GetServerRunning;
  end;

implementation

uses
  uXplCommon, XPLMUtilities;

{ TXplSender }

procedure TXplSender.DebugFmt(pFormat: String; pArgs: array of const);
begin
  if Assigned(fDebugMethod) then
    fDebugMethod(pFormat, pArgs);
end;

procedure TXplSender.DumpStream(pStream: TStream);
var i: Integer;
  b: Byte;
begin
  pStream.Position:=0;
  for I := 0 to pStream.Size - 1 do
  begin
    b := pStream.ReadByte;
    DebugFmt('%d: dec: %d, hex: %x, char %s', [i, b, b, Chr(b)]);
  end;
end;

function TXplSender.GetServerRunning: Boolean;
begin
  Result := fClient.ServerRunning;
end;

constructor TXplSender.Create(pName: String);
begin
  fClient := TSimpleIPCClient.Create(nil);
  fClient.ServerID:=pName;
  fDebugMethod:=nil;
end;

destructor TXplSender.Destroy;
begin
  if (fClient.Active) then
    fClient.Disconnect;
  fClient.Free;
  inherited Destroy;
end;

procedure TXplSender.SendString(pValue: String);
begin
  DebugFmt('Sending string: %s', [pValue]);
  if (not fClient.Active) then
  begin
    DebugFmt('Will try to connect', []);
    if (fClient.ServerRunning) then
    begin
      fClient.Connect;
    end
    else
      DebugFmt('Server is not running, connection aborted', []);
  end;
  if (fClient.Active) then
  begin
    DebugFmt('Connected, sending value', []);
    fClient.SendStringMessage(pValue);
  end
  else
    DebugFmt('Not connected', []);
end;

procedure TXplSender.SendMessage(pMessage: TXplMessage);
var
  lStream: TMemoryStream;
begin
  if (not fClient.Active) then
  begin
    DebugFmt('IPC sender is not connected, will try to connect now', []);
    if (fClient.ServerRunning) then
    begin
      fClient.Connect;
    end
    else
      DebugFmt('IPC listener is not running, connection aborted', []);
  end;
  if (fClient.Active) then
  begin
    DebugFmt('IPC sender is connected, sending value...', []);
    lStream:=TMemoryStream.Create;
    try
      pMessage.SerializeToStream(lStream);
      fClient.SendMessage(mtUnknown,lStream);
      DebugFmt('Sent value through IPC stream with size %d', [lStream.Size]);
      //DumpStream(lStream);
    finally
      lStream.free;
    end;
  end
  else
    DebugFmt('IPC sender is still not connected. Sending aborted.', []);
end;

procedure TXplSender.Reconnect;
begin
  if (fClient.Active) then
  begin
    fClient.Active:= False; // will connect on next send
    DebugFmt('Reconnect request, disconnecting from server, connection will be set up on next send.', []);
  end
  else
    DebugFmt('Reconnect request but connection is not established. Nothing to be done.', []);
end;

end.

