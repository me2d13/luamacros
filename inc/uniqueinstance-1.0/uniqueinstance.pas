unit UniqueInstance;

{
  UniqueInstance is a component to allow only a instance by program

  Copyright (C) 2006 Luiz Americo Pereira Camara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, simpleipc, ExtCtrls;
  
type

  TOnOtherInstance = procedure (Sender : TObject; ParamCount: Integer; Parameters: array of String) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    FIdentifier: String;
    FIPCServer: TSimpleIPCServer;
    FOnOtherInstance: TOnOtherInstance;
    FUpdateInterval: Cardinal;
    FEnabled: Boolean;
    function GetServerId: String;
    procedure ReceiveMessage(Sender: TObject);
    procedure TerminateApp(Sender: TObject; var Done: Boolean);
    {$ifdef unix}
    procedure CheckMessage(Sender: TObject);
    {$endif}
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Identifier: String read FIdentifier write FIdentifier;
    property UpdateInterval: Cardinal read FUpdateInterval write FUpdateInterval default 1000;
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

implementation

uses
  StrUtils, SimpleIPCWrapper;

const
  BaseServerId = 'tuniqueinstance_';
  Separator = '|';

function GetFormattedParams: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStr(i) + Separator;
end;

{ TUniqueInstance }

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  TempArray: array of String;
  Count,i: Integer;

  procedure GetParams(const AStr: String);
  var
    pos1,pos2:Integer;
  begin
    SetLength(TempArray, Count);
    //fill params
    i := 0;
    pos1:=1;
    pos2:=pos(Separator, AStr);
    while pos1 < pos2 do
    begin
      TempArray[i] := Copy(AStr, pos1, pos2 - pos1);
      pos1 := pos2+1;
      pos2 := posex(Separator, AStr, pos1);
      inc(i);
    end;
  end;

begin
  if Assigned(FOnOtherInstance) then
  begin
    //MsgType stores ParamCount
    Count := FIPCServer.MsgType;
    GetParams(FIPCServer.StringMessage);
    FOnOtherInstance(Self, Count, TempArray);
    SetLength(TempArray, 0);
  end;
end;

{$ifdef unix}
procedure TUniqueInstance.CheckMessage(Sender: TObject);
begin
  FIPCServer.PeekMessage(1, True);
end;
{$endif}

procedure TUniqueInstance.TerminateApp(Sender: TObject; var Done: Boolean);
begin
  Application.Terminate;
  //necessary to avoid being a zombie
  Done := False;
end;

function TUniqueInstance.GetServerId: String;
begin
  if FIdentifier <> '' then
    Result := BaseServerId + FIdentifier
  else
    Result := BaseServerId + ExtractFileName(ParamStr(0));
end;


procedure TUniqueInstance.Loaded;
var
  IPCClient: TSimpleIPCClient;
  {$ifdef unix}
  Timer: TTimer;
  {$endif}
begin
  if not (csDesigning in ComponentState) and FEnabled then
  begin
    IPCClient := TSimpleIPCClient.Create(Self);
    IPCClient.ServerId := GetServerId;
    if IsServerRunning(IPCClient) then
    begin
      //A instance is already running
      //Send a message and then exit
      if Assigned(FOnOtherInstance) then
      begin
        IPCClient.Active := True;
        IPCClient.SendStringMessage(ParamCount, GetFormattedParams);
      end;
      Application.ShowMainForm := False;
      //Calling Terminate directly here would cause a crash under gtk2 in LCL < 0.9.31
      //todo: remove the workaround after a release with LCL > 0.9.31
      //Application.Terminate;

      //calling as an async call will not work also since it will lead to a zombie process
      //Application.QueueAsyncCall(@TerminateApp, 0);

      //New try:
      Application.AddOnIdleHandler(@TerminateApp);
    end
    else
    begin
      //It's the first instance. Init the server
      FIPCServer := TSimpleIPCServer.Create(Self);
      FIPCServer.ServerID := IPCClient.ServerId;
      FIPCServer.Global := True;
      FIPCServer.OnMessage := @ReceiveMessage;
      InitServer(FIPCServer);
      //there's no more need for IPCClient
      IPCClient.Destroy;
      {$ifdef unix}
      if Assigned(FOnOtherInstance) then
      begin
        Timer := TTimer.Create(Self);
        Timer.Interval := FUpdateInterval;
        Timer.OnTimer := @CheckMessage;
      end;
      {$endif}
    end;
  end;//if
  inherited;
end;

constructor TUniqueInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateInterval := 1000;
end;

end.

