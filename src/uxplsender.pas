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
    public
      constructor Create(pName: String);
      destructor Destroy; Override;
      procedure SendString(pValue: String);
      procedure SendMessage(pMessage: TXplMessage);
      property DebugMethod: TDebugMethod read fDebugMethod write fDebugMethod;
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
  lStream: TBytesStream;
begin
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
    lStream:=TBytesStream.Create;
    try
      pMessage.SerializeToStream(lStream);
      fClient.SendMessage(mtUnknown,lStream);
    finally
      lStream.free;
    end;
  end
  else
    DebugFmt('Not connected', []);
end;

end.

