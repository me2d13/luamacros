unit uComDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazSerial, uDevice;

type

    { TComDevice }

    TComDevice = class(TDevice)
    private
      fComPort: TLazSerial;
      fSpeed: TBaudRate;
      fParity: TParity;
      fDataBits: TDataBits;
      fStopBits: TStopBits;
      fCustomValues: Boolean;
      fSeparator: String;
      fBuffer: String;
      function GetActive: Boolean;
      function GetDataBits: Integer;
      function GetParity: String;
      function GetSpeed: Integer;
      function GetStopBits: Integer;
      procedure OnRxData(Sender: TObject);
      procedure SetActive(AValue: Boolean);
      procedure SetDataBits(AValue: Integer);
      procedure SetParity(AValue: String);
      procedure SetSeparator(AValue: String);
      procedure SetSpeed(AValue: Integer);
      procedure SetStopBits(AValue: Integer);
      procedure SplitBuffer;
      procedure FlushBuffer;
    public
      constructor Create;
      destructor Destroy;override;
      function TypeCaption: String; override;
      procedure Init;
      procedure SendData(pData: String);
      property Active: Boolean read GetActive write SetActive;
      property Speed:Integer read GetSpeed write SetSpeed;
      property Parity:String read GetParity write SetParity;
      property DataBits:Integer read GetDataBits write SetDataBits;
      property StopBits:Integer read GetStopBits write SetStopBits;
      property Separator:String read fSeparator write SetSeparator;
  end;

  TSerialException = class (Exception);
  TWrongSerialConfigurationException = class (TSerialException);


implementation

uses
  uMainFrm, uGlobals;

const
  cBufferMaxSize = 2048;

{ TComDevice }

procedure TComDevice.OnRxData(Sender: TObject);
var
  lData: String;
begin
  lData := fComPort.ReadData;
  Glb.DebugLog('Received data from COM port ' + SystemId + ': ' + lData, cLoggerCom);
  if (fSeparator = '') then
  begin
    Glb.LuaEngine.OnDeviceEvent(self, lData);
  end
  else
  begin
    if (Length(fBuffer) + Length(lData) > cBufferMaxSize) then
    begin
      Glb.DebugLogFmt('Rx buffer %s overflow, can''t split.', [Name], cLoggerCom);
      FlushBuffer;
    end;
    fBuffer:=fBuffer + lData;
    SplitBuffer;
  end;
end;

function TComDevice.GetActive: Boolean;
begin
  Result := (fComPort <> nil) and fComPort.Active;
end;

function TComDevice.GetDataBits: Integer;
begin
  if (fComPort <> nil) then
    fDataBits:=fComPort.DataBits;
  Result := ConstsBits[fDataBits];
end;

function TComDevice.GetParity: String;
begin
  if (fComPort <> nil) then
    fParity:=fComPort.Parity;
  Result := ConstsParity[fParity];
end;

function TComDevice.GetSpeed: Integer;
begin
  if (fComPort <> nil) then
    fSpeed:=fComPort.BaudRate;
  Result := ConstsBaud[fSpeed];
end;

function TComDevice.GetStopBits: Integer;
begin
  if (fComPort <> nil) then
    fStopBits:=fComPort.StopBits;
  Result := ConstsStopBits[fStopBits];
  if (Result = 0) then
    Result := 1; // we do not support 1.5 now, so make 1 and 1.5 same value (1)
end;

procedure TComDevice.SetActive(AValue: Boolean);
begin
  Glb.DebugLog('Setting port '+Name+' active ' + BoolToStr(AValue, 'TRUE', 'FALSE'), cLoggerCom);
  if (fComPort = nil) then
    Init;
  if (fComPort <> nil) then
    fComPort.Active:=AValue;
end;

procedure TComDevice.SetDataBits(AValue: Integer);
var
  lVal: TDataBits;
begin
  for lVal in TDataBits do
    if ConstsBits[lVal] = AValue then
    begin
      fDataBits:=lVal;
      fCustomValues:=True;
      exit;
    end;
  raise TWrongSerialConfigurationException.CreateFmt('Wrong data bits value %d', [AValue]);
end;

procedure TComDevice.SetParity(AValue: String);
var
  lVal: TParity;
begin
  for lVal in TParity do
    if ConstsParity[lVal] = AValue then
    begin
      fParity:=lVal;
      fCustomValues:=True;
      exit;
    end;
  raise TWrongSerialConfigurationException.CreateFmt('Wrong parity value %s', [AValue]);
end;

procedure TComDevice.SetSeparator(AValue: String);
begin
  if fSeparator=AValue then Exit;
  fSeparator:=AValue;
  if (fSeparator = '') then
  begin
    // turning separator feature off, clear buffer
    FlushBuffer;
  end
  else
  begin
    Glb.DebugLogFmt('Setting separator of port %s to "%s"', [Name, fSeparator], cLoggerCom);
  end;
end;

procedure TComDevice.SetSpeed(AValue: Integer);
var
  lVal: TBaudRate;
begin
  for lVal in TBaudRate do
    if ConstsBaud[lVal] = AValue then
    begin
      fSpeed:=lVal;
      fCustomValues:=True;
      exit;
    end;
  raise TWrongSerialConfigurationException.CreateFmt('Wrong baud rate value %d', [AValue]);
end;

procedure TComDevice.SetStopBits(AValue: Integer);
begin
  if (AValue = 1) then
  begin
    fStopBits:=sbOne;
    fCustomValues:=True;
  end
  else if (AValue = 2) then
  begin
    fStopBits:=sbTwo;
    fCustomValues:=True;
  end
  else
    raise TWrongSerialConfigurationException.CreateFmt('Wrong stop bits value %d', [AValue]);
end;

procedure TComDevice.SplitBuffer;
var
  lPos: Integer;
  lChunk: String;
begin
  if (fSeparator = '') or (fBuffer = '') then
    exit;
  lPos := Pos(fSeparator, fBuffer);
  while (lPos > 0) do
  begin
    lChunk:=Copy(fBuffer, 1, lPos - 1);
    Glb.DebugLogFmt('Callback with COM data "%s" separated by splitter.', [lChunk], cLoggerCom);
    Glb.LuaEngine.OnDeviceEvent(self, lChunk);
    fBuffer:=Copy(fBuffer, lPos + Length(fSeparator), cBufferMaxSize);
    lPos := Pos(fSeparator, fBuffer);
  end;
  if Length(fBuffer) > 0 then
    Glb.DebugLogFmt('Received data kept in buffer of COM %s. Buffer content: "%s"', [Name, fBuffer], cLoggerCom);
end;

procedure TComDevice.FlushBuffer;
begin
  if (Length(fBuffer) > 0) then
  begin
    Glb.DebugLogFmt('Flushing buffer of port %s with data: %s.', [Name, fBuffer], cLoggerCom);
    Glb.LuaEngine.OnDeviceEvent(self, fBuffer);
    fBuffer:='';
  end;
end;

constructor TComDevice.Create;
begin
  fComPort := nil;
  fCustomValues := False; // can handle all 4 as they can not be set separately
  fSeparator := '';
end;

destructor TComDevice.Destroy;
begin
  if (fComPort <> nil) then
    fComPort.Free;
  inherited Destroy;
end;

function TComDevice.TypeCaption: String;
begin
  Result:='COM';
end;

procedure TComDevice.Init;
begin
  fComPort := TLazSerial.Create(gMainForm);
  fComPort.Device:=SystemId;
  if (fCustomValues) then
  begin
    fComPort.BaudRate:=fSpeed;
    fComPort.DataBits:=fDataBits;
    fComPort.Parity:=fParity;
    fComPort.StopBits:=fStopBits;
  end;
  fComPort.Active:=True;
  // callback
  fComPort.OnRxData:=OnRxData;
end;

procedure TComDevice.SendData(pData: String);
begin
  if (fComPort = nil) or (not fComPort.Active) then
    Init;
  if (fComPort = nil) or (not fComPort.Active) then
    raise TSerialException.Create('Can not send data, COM port is not active.');
  fComPort.WriteData(pData);
end;

end.

