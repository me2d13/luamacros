unit uXplAbstractReceiver;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleIpc;

type

  { TXplAbstractListener }

  TXplAbstractListener = class abstract(TObject)
    private
      fServer: TSimpleIPCServer;
      procedure OnXplMessage(Sender: TObject); virtual; abstract;
    public
      constructor Create(pName: String);
      destructor Destroy; Override;
      procedure Init;
  end;


implementation

{ TXplAbstractListener }

constructor TXplAbstractListener.Create(pName: String);
begin
  fServer := TSimpleIPCServer.Create(nil);
  fServer.ServerID:=pName;
  fServer.OnMessage:=OnXplMessage;
  fServer.Global:=true;
end;

destructor TXplAbstractListener.Destroy;
begin
  if (fServer.Active) then
    fServer.StopServer;
  fServer.Free;
  inherited Destroy;
end;

procedure TXplAbstractListener.Init;
begin
  fServer.StartServer;
end;

end.

