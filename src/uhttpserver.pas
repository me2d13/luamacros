unit uHttpServer;

{$mode delphi}

interface

uses
  blcksock, sockets, Synautil, Classes, SysUtils, lnet, lhttp, lHTTPUtil;

type

  { THttpServer }

  THttpServer = class (TThread)
    private
      fPort: Integer;
      fHandler: Integer;
      procedure SetPort(AValue: Integer);
    public
      constructor Create(CreateSuspended: Boolean; pPort: Integer; pHandler: Integer);
      property Port: Integer read FPort write SetPort;
    protected
      procedure Execute; override;
      procedure AttendConnection(pSocket: TSocket);
      function CreateOkAnswer: String;
      function CreateBackAnswer: String;
  end;

  { THttpService }

  THttpService = class
    private
      fHttpServer: THttpServer;
    public
      constructor Create;
      destructor Destroy;override;
      procedure StartServer(pPort: Integer; pHandlerRef: Integer);
      procedure StopServer;
      function HttpGet(pUrl: String; pTimeout: Integer): String;
  end;

  { THTTPHandler }

  THTTPHandler = class
  private
    fResult: String;
    fDone: Boolean;
  public
    constructor Create;
    procedure ClientDisconnect(ASocket: TLSocket);
    procedure ClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure ClientError(const Msg: string; aSocket: TLSocket);
    function ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: Integer): Integer;
    procedure ClientProcessHeaders(ASocket: TLHTTPClientSocket);
    property Result: String read fResult;
    property Done: Boolean read fDone;
  end;


implementation

uses
  uGlobals, uLuaEngine,lnetSSL, URIParser;

{ THTTPHandler }

constructor THTTPHandler.Create;
begin
  fDone := False;
end;

procedure THTTPHandler.ClientDisconnect(ASocket: TLSocket);
begin
  Glb.DebugLog('Http client disconnected', cLoggerHtp);
  fDone:=True;
end;

procedure THTTPHandler.ClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  Glb.DebugLog('Http client done input reading', cLoggerHtp);
  ASocket.Disconnect;
end;

procedure THTTPHandler.ClientError(const Msg: string; aSocket: TLSocket);
begin
  Glb.LogError('Http client error ' + Msg, cLoggerHtp);
  fDone:=True;
end;

function THTTPHandler.ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: Integer): Integer;
var
  lChunk: String;
begin
  // add to result string
  SetString(lChunk, ABuffer, ASize);
  Glb.DebugLogFmt('Received part size %d: %s', [ASize, lChunk], cLoggerHtp);
  fResult:=fResult+lChunk;
  Result := ASize;
end;

procedure THTTPHandler.ClientProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  Glb.DebugLogFmt('Response %d %s', [HTTPStatusCodes[ASocket.ResponseStatus], ASocket.ResponseReason], cLoggerHtp);
end;

{ THttpServer }

procedure THttpServer.SetPort(AValue: Integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

constructor THttpServer.Create(CreateSuspended: Boolean; pPort: Integer; pHandler: Integer);
begin
  inherited Create(CreateSuspended);
  fPort:= pPort;
  fHandler:= pHandler;
  FreeOnTerminate:=False;
end;

procedure THttpServer.Execute;
var
  ListenerSocket: TTCPBlockSocket;
  ConnectionSocket: TSocket;
begin
  ListenerSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('0.0.0.0',IntToStr(fPort));
  ListenerSocket.listen;

  repeat
    if ListenerSocket.canread(100) then
    begin
      ConnectionSocket := ListenerSocket.accept;
      AttendConnection(ConnectionSocket);
    end;
  until Terminated;

  ListenerSocket.Free;
end;

procedure THttpServer.AttendConnection(pSocket: TSocket);
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  ResultCode: integer;
  lSocket: TTCPBlockSocket;
  lAnswer: String;
  lResult: TLuaResult;
  lContentType: String;
begin
  lSocket := TTCPBlockSocket.Create;
  lSocket.Socket:=pSocket;
  timeout := 120000;

  //read request line
  s := lSocket.RecvString(timeout);
  if (lSocket.LastError <> 0) then
  begin
    //Glb.DebugLog('Socket error: ' + lSocket.LastErrorDesc, cLoggerHtp);
    lSocket.Free;
    exit;
  end;
  if (s = '') then
  begin
    //Glb.DebugLog('No data on socket read', cLoggerHtp);
    lSocket.Free;
    exit;
  end;

  Glb.DebugLog('Incoming request: ' + s, cLoggerHtp);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');

  if (Copy(uri, 1, 5) = '/back') then
  begin
    uri := Copy(uri, 6, 100000);
    lAnswer := CreateBackAnswer;
  end else begin
    lAnswer := CreateOkAnswer;
  end;

  //Glb.DebugLog('Received headers+document requesting ' + uri, cLoggerHtp);

  //read request headers
  repeat
    s := lSocket.RecvString(Timeout);
  until s = '';

  // Call LUA code
  Glb.DebugLog(Format('Calling Lua function %d with argument %s.', [fHandler, uri]), cLoggerHtp);
  lResult := Glb.LuaEngine.CallFunctionByRefWithResult(fHandler, uri);
  Glb.DebugLogFmt('Got function results with %d items.', [Length(lResult.Value)], cLoggerHtp);

  lContentType:='Text/Html';
  if lResult <> nil then
  begin
    if Length(lResult.Value) > 0 then
    begin
      lAnswer:=lResult.Value[0];
      if Length(lResult.Value) > 1 then
      begin
        // parameters are returned in reverse order
        lContentType:=lAnswer;
        lAnswer:=lResult.Value[1];
      end;
    end;
  end;

    // Write the headers back to the client
    lSocket.SendString('HTTP/1.0 200' + CRLF);
    lSocket.SendString('Content-type: '+ lContentType + CRLF);
    lSocket.SendString('Content-length: ' + IntTostr(Length(lAnswer)) + CRLF);
    lSocket.SendString('Connection: close' + CRLF);
    lSocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    lSocket.SendString('Server: LuaMacros' + CRLF);
    lSocket.SendString('Access-Control-Allow-Origin: *' + CRLF);
    lSocket.SendString('' + CRLF);

    lSocket.SendString(lAnswer);
    //ASocket.SendString('HTTP/1.0 404' + CRLF);
    lSocket.Free;
end;

function THttpServer.CreateOkAnswer: String;
begin
  Result :=
      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
      + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
      + '<html>OK</html>' + CRLF;
end;

function THttpServer.CreateBackAnswer: String;
begin
  Result :=
      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
      + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
      + '<html><script>window.history.back();</script></html>' + CRLF;
end;


{ THttpService }

constructor THttpService.Create;
begin
  fHttpServer:=nil;
end;

destructor THttpService.Destroy;
begin
  StopServer;
  inherited Destroy;
end;

procedure THttpService.StartServer(pPort: Integer; pHandlerRef: Integer);
begin
  Glb.DebugLogFmt('Starting HTTP server at port %d.', [pPort], cLoggerHtp);
  fHttpServer := THttpServer.Create(true, pPort, pHandlerRef);
  fHttpServer.Start;
end;

procedure THttpService.StopServer;
begin
  if (fHttpServer <> nil) then
  begin
    Glb.DebugLogFmt('Stopping HTTP server at port %d.', [fHttpServer.Port], cLoggerHtp);
    fHttpServer.Terminate;
    fHttpServer.WaitFor;
    fHttpServer.Free;
  end;
end;

function THttpService.HttpGet(pUrl: String; pTimeout: Integer): String;
var
  lHttpHandler: THTTPHandler;
  Host, URI: string;
  Port: Word;
  UseSSL: Boolean;
  SSLSession: TLSSLSession;
  HttpClient: TLHTTPClient;
begin
  UseSSL := DecomposeURL(pUrl, Host, URI, Port);
  HttpClient := TLHTTPClient.Create(nil);
  lHttpHandler := THTTPHandler.Create;
  if (UseSSL) then
  begin
    SSLSession := TLSSLSession.Create(HttpClient);
    HttpClient.Session := SSLSession;
    SSLSession.SSLActive := UseSSL;
  end;
  HttpClient.Host := Host;
  HttpClient.Method := hmGet;
  HttpClient.Port := Port;
  HttpClient.URI := URI;
  HttpClient.Timeout := pTimeout;
  HttpClient.OnDisconnect := lHttpHandler.ClientDisconnect;
  HttpClient.OnDoneInput := lHttpHandler.ClientDoneInput;
  HttpClient.OnError := lHttpHandler.ClientError;
  HttpClient.OnInput := lHttpHandler.ClientInput;
  HttpClient.OnProcessHeaders := lHttpHandler.ClientProcessHeaders;
  HttpClient.SendRequest;

  while not lHttpHandler.Done do
    HttpClient.CallAction;
  Result := lHttpHandler.Result;
  lHttpHandler.Free;
  HttpClient.Free;
end;

end.

