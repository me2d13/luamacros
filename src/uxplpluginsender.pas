unit uXplPluginSender;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleIpc;

type

  { TXplSender }

  TXplSender = class
    private
      fClient: TSimpleIPCClient;
    public
      constructor Create;
      destructor Destroy; Override;
      procedure SendString(pValue: String);
      procedure XplDebugFmt(pFormat:String; pArgs: array of const);
  end;

implementation

uses
  uXplCommon, XPLMUtilities;

{ TXplSender }

constructor TXplSender.Create;
begin
  fClient := TSimpleIPCClient.Create(nil);
  fClient.ServerID:=cServerName;
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
  XplDebugFmt('Sending string: %s', [pValue]);
  if (not fClient.Active) then
  begin
    XplDebugFmt('Will try to connect', []);
    if (fClient.ServerRunning) then
    begin
      fClient.Connect;
    end
    else
      XplDebugFmt('Server is not running, connection aborted', []);
  end;
  if (fClient.Active) then
  begin
    XplDebugFmt('Connected, sending value', []);
    fClient.SendStringMessage(pValue);
  end
  else
    XplDebugFmt('Not connected', []);
end;

procedure TXplSender.XplDebugFmt(pFormat: String; pArgs: array of const);
var
 lBuff: PChar;
 lMessage: String;
begin
  lMessage:=Format(pFormat, pArgs) + #13;
  GetMem(lBuff, Length(lMessage) + 1);
  try
    StrPCopy(lBuff, lMessage);
    XPLMDebugString(lBuff);
  finally
    FreeMem(lBuff);
  end;
end;

end.

