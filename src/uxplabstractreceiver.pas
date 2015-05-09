unit uXplAbstractReceiver;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleIpc;

type

  { TXplAbstractReceiver }

  TXplAbstractReceiver = class (TObject)
    private
      fServer: TSimpleIPCServer;
      function GetOnMessage: TNotifyEvent;
      procedure SetOnMessage(AValue: TNotifyEvent);
    public
      constructor Create(pName: String);
      destructor Destroy; Override;
      procedure Init;
      Property OnMessage : TNotifyEvent Read GetOnMessage Write SetOnMessage;
      property Server: TSimpleIPCServer read fServer;
  end;


implementation

{ TXplAbstractReceiver }

procedure TXplAbstractReceiver.SetOnMessage(AValue: TNotifyEvent);
begin
  fServer.OnMessage:=AValue;
end;

function TXplAbstractReceiver.GetOnMessage: TNotifyEvent;
begin
  Result := fServer.OnMessage;
end;

constructor TXplAbstractReceiver.Create(pName: String);
begin
  fServer := TSimpleIPCServer.Create(nil);
  fServer.ServerID:=pName;
  fServer.Global:=true;
end;

destructor TXplAbstractReceiver.Destroy;
begin
  if (fServer.Active) then
    fServer.StopServer;
  fServer.Free;
  inherited Destroy;
end;

procedure TXplAbstractReceiver.Init;
begin
  fServer.StartServer;
end;

end.

