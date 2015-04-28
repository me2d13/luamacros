unit uXplListener;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleIpc;

type

  { TXplListener }

  TXplListener = class
    private
      fServer: TSimpleIPCServer;
      procedure OnXplMessage(Sender: TObject);
    public
      constructor Create;
      destructor Destroy; Override;
      procedure Init;
  end;

implementation

uses
  uGlobals, uXplCommon;

{ TXplListener }

procedure TXplListener.OnXplMessage(Sender: TObject);
begin
  Glb.DebugLog(fServer.StringMessage, cLoggerXpl);
end;

constructor TXplListener.Create;
begin
  fServer := TSimpleIPCServer.Create(nil);
  fServer.ServerID:=cServerName;
  fServer.OnMessage:=OnXplMessage;
  fServer.Global:=true;
end;

destructor TXplListener.Destroy;
begin
  fServer.StopServer;
  fServer.Free;
  inherited Destroy;
end;

procedure TXplListener.Init;
begin
  fServer.StartServer;
  if (fServer.Active) then
    Glb.LogError('Xpl listener started', cLoggerXpl)
  else
    Glb.LogError('Xpl listener not started', cLoggerXpl);
end;

end.

