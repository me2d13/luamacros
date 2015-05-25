unit uXplMessages;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TXplMessage }

  TXplMessage = class
    procedure SerializeToStream(pStream: TStream); virtual;
  end;

  { TXplDrawText }

  TXplDrawText = class (TXplMessage)
  private
    fText: String;
    fPosition: Single;
    fTimeInSec: Integer;
  public
    constructor Create(pText: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Position: Single read fPosition write fPosition;
    property TimeInSec: Integer read fTimeInSec write fTimeInSec;
    property Text: String read fText write fText;
  end;

  TXplVarType = (vtNull, vtInteger, vtDouble, vtString);

  { TXplValue }

  TXplValue = class
  private
    fType: TXplVarType;
    fIntValue: Int64;
    fDoubleValue: Double;
    fStringValue: String;
    procedure SetDoubleValue(AValue: Double);
    procedure SetIntValue(AValue: Int64);
    procedure SetStringValue(AValue: String);
  public
    constructor Create;overload;
    constructor Create(pIntValue: Integer);overload;
    constructor Create(pDoubleValue: Double);overload;
    constructor Create(pStringValue: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);virtual;
    procedure MakeDouble;
    procedure MakeInt;
    procedure MakeString;
    function ToString: ansistring;override;
    function Equals(pVar: TXplValue): boolean;
    property IntValue: Int64 read fIntValue write SetIntValue;
    property DoubleValue: Double read fDoubleValue write SetDoubleValue;
    property StringValue: String read fStringValue write SetStringValue;
    property ValueType: TXplVarType read fType;
  end;

  { TXplSetVariable }

  TXplSetVariable = class (TXplMessage)
  protected
    fName: String;
    fValue: TXplValue;
    procedure WriteIdentByte(pStream: TStream);virtual;
  public
    constructor Create(pName: String; pValue: TXplValue);overload;
    constructor Create(pStream: TStream);overload;
    destructor Destroy;override;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property Value: TXplValue read fValue write fValue;
  end;

  { TXplGetVariable }

  TXplGetVariable = class (TXplMessage)
  private
    fName: String;
    fId: Int64;
  public
    constructor Create(pName: String; pId: Int64);overload;
    constructor Create(pStream: TStream);overload;
    destructor Destroy;override;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property Id: Int64 read fId write fId;
  end;

  { TXplVariableValue }

  TXplVariableValue = class (TXplSetVariable)
  protected
    fId: Int64;
    fChangeCount: Int64; // how many times variable was changed between 2 messages
    procedure WriteIdentByte(pStream: TStream);override;
  public
    constructor Create(pName: String; pValue: TXplValue; pId: Int64);overload;
    constructor Create(pName: String; pValue: TXplValue; pId: Int64; pChangeCount: Int64);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    function ToString: ansistring;override;
    property Id: Int64 read fId write fId;
    property ChangeCount: Int64 read fChangeCount write fChangeCount;
  end;

  { TXplReconnectToServer }

  TXplReconnectToServer = class (TXplMessage)
  public
    procedure SerializeToStream(pStream: TStream);override;
  end;

  { TXplExecuteCommand }

  TXplExecuteCommand = class (TXplMessage)
  protected
    fName: String;
    procedure WriteIdentByte(pStream: TStream);virtual;
  public
    constructor Create(pName: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
  end;

  { TXplExecuteCommandBegin }

  TXplExecuteCommandBegin = class (TXplExecuteCommand)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplExecuteCommandEnd }

  TXplExecuteCommandEnd = class (TXplExecuteCommand)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplVariableCallback }

  TXplVariableCallback = class (TXplMessage)
  private
    fName: String;
    fIntervalMs: Int64;
    fId: Int64;
  public
    constructor Create(pName: String; pIntervalMs: Int64; pId: Int64);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property IntervalMs: Int64 read fIntervalMs write fIntervalMs;
    property Id: Int64 read fId write fId;
  end;


implementation

uses
  uXplCommon;

{ TXplVariableCallback }

constructor TXplVariableCallback.Create(pName: String; pIntervalMs: Int64; pId: Int64);
begin
  fName:=pName;
  fIntervalMs:=pIntervalMs;
  fId := pId;
end;

constructor TXplVariableCallback.Create(pStream: TStream);
begin
  fName:=pStream.ReadAnsiString;
  pStream.Read(fId, SizeOf(fId));
  pStream.Read(fIntervalMs, SizeOf(fIntervalMs));
end;

procedure TXplVariableCallback.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(HDMC_VAR_CALLBACK);
  pStream.WriteAnsiString(fName);
  pStream.Write(fId, SizeOf(fId));
  pStream.Write(fIntervalMs, SizeOf(fIntervalMs));
end;

{ TXplExecuteCommandEnd }

procedure TXplExecuteCommandEnd.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_COMMAND_END);
end;

{ TXplExecuteCommandBegin }

procedure TXplExecuteCommandBegin.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_COMMAND_BEGIN);
end;

{ TXplExecuteCommand }

procedure TXplExecuteCommand.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_EXEC_COMMAND);
end;

constructor TXplExecuteCommand.Create(pName: String);
begin
  fName:=pName;
end;

constructor TXplExecuteCommand.Create(pStream: TStream);
begin
  fName:=pStream.ReadAnsiString;
end;

procedure TXplExecuteCommand.SerializeToStream(pStream: TStream);
begin
  WriteIdentByte(pStream);
  pStream.WriteAnsiString(fName);
end;

{ TXplReconnectToServer }

procedure TXplReconnectToServer.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(HDMC_RECONNECT);
end;

{ TXplVariableValue }

procedure TXplVariableValue.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_VAR_RESPONSE);
end;

constructor TXplVariableValue.Create(pName: String; pValue: TXplValue;
  pId: Int64);
begin
  inherited Create(pName, pValue);
  fId:=pId;
end;

constructor TXplVariableValue.Create(pName: String; pValue: TXplValue;
  pId: Int64; pChangeCount: Int64);
begin
  Create(pName, pValue, pId);
  fChangeCount:=pChangeCount;
end;

constructor TXplVariableValue.Create(pStream: TStream);
begin
  inherited;
  pStream.Read(fId, SizeOf(fId));
end;

procedure TXplVariableValue.SerializeToStream(pStream: TStream);
begin
  inherited SerializeToStream(pStream);
  pStream.Write(fId, SizeOf(fId));
end;

function TXplVariableValue.ToString: ansistring;
begin
  Result := Format('name "%s", id %d, value ', [fName, fId]);
  if (fValue = nil) then
    Result := Result + 'nil'
  else
    Result := Result + fValue.ToString;
end;

{ TXplGetVariable }

constructor TXplGetVariable.Create(pName: String; pId: Int64);
begin
  fName:=pName;
  fId:=pId;
end;

constructor TXplGetVariable.Create(pStream: TStream);
begin
  fName := pStream.ReadAnsiString;
  pStream.Read(fId, SizeOf(fId));
end;

destructor TXplGetVariable.Destroy;
begin
  inherited Destroy;
end;

procedure TXplGetVariable.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(HDMC_GET_VAR);
  pStream.WriteAnsiString(fName);
  pStream.Write(fId, SizeOf(fId));
end;

{ TXplValue }

procedure TXplValue.SetDoubleValue(AValue: Double);
begin
  fDoubleValue:=AValue;
  fType:=vtDouble;
end;

procedure TXplValue.SetIntValue(AValue: Int64);
begin
  fIntValue:=AValue;
  fType:=vtInteger;
end;

procedure TXplValue.SetStringValue(AValue: String);
begin
  fStringValue:=AValue;
  fType:=vtString;
end;

constructor TXplValue.Create;
begin
  fType:=vtNull;
end;

constructor TXplValue.Create(pIntValue: Integer);
begin
  SetIntValue(pIntValue);
end;

constructor TXplValue.Create(pDoubleValue: Double);
begin
  SetDoubleValue(pDoubleValue);
end;

constructor TXplValue.Create(pStringValue: String);
begin
  SetStringValue(pStringValue);
end;

constructor TXplValue.Create(pStream: TStream);
begin
  fType:=TXplVarType(pStream.ReadByte);
  case fType of
    vtInteger: pStream.Read(fIntValue, SizeOf(fIntValue));
    vtDouble: pStream.Read(fDoubleValue, SizeOf(fDoubleValue));
    vtString: fStringValue := pStream.ReadAnsiString;
  end;
end;

procedure TXplValue.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(Ord(fType));
  case fType of
    vtInteger: pStream.Write(fIntValue, SizeOf(fIntValue));
    vtDouble: pStream.Write(fDoubleValue, SizeOf(fDoubleValue));
    vtString: pStream.WriteAnsiString(fStringValue);
  end;
end;

procedure TXplValue.MakeDouble;
begin
  if (fType = vtDouble) then
    exit;
  case fType of
    vtInteger: SetDoubleValue(fIntValue);
    vtString: SetDoubleValue(StrToFloat(fStringValue));
    else SetDoubleValue(0);
  end;
end;

procedure TXplValue.MakeInt;
begin
  if (fType = vtInteger) then
    exit;
  case fType of
    vtDouble: SetIntValue(trunc(fDoubleValue));
    vtString: SetIntValue(StrToInt(fStringValue));
    else SetIntValue(0);
  end;
end;

procedure TXplValue.MakeString;
begin
  if (fType = vtString) then
    exit;
  case fType of
    vtDouble: SetStringValue(FloatToStr(fDoubleValue));
    vtInteger: SetStringValue(IntToStr(fIntValue));
    else SetStringValue('');
  end;
end;

function TXplValue.ToString: ansistring;
begin
  Result := 'N/A';
  case fType of
    vtNull: Result := 'null';
    vtString: Result := '[string] "' + fStringValue + '"';
    vtInteger: Result := '[int] ' + IntToStr(fIntValue);
    vtDouble: Result := '[double] ' + FloatToStr(fDoubleValue);
  end;
end;

function TXplValue.Equals(pVar: TXplValue): boolean;
begin
  Result := False;
  if (pVar = nil) then exit;
  if (fType <> pVar.ValueType) then exit;
  Result :=
    ((fType = vtInteger) and (fIntValue = pVar.IntValue)) or
    ((fType = vtDouble) and (fDoubleValue = pVar.DoubleValue)) or
    ((fType = vtString) and (fStringValue = pVar.StringValue)) or
    ((fType = vtNull));
end;

{ TXplSetVariable }

procedure TXplSetVariable.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_SET_VAR);
end;

constructor TXplSetVariable.Create(pName: String; pValue: TXplValue);
begin
  fName:=pName;
  fValue := pValue;
end;

constructor TXplSetVariable.Create(pStream: TStream);
begin
  fName := pStream.ReadAnsiString;
  fValue := TXplValue.Create(pStream);
end;

destructor TXplSetVariable.Destroy;
begin
  if (fValue <> nil) then
    fValue.Free;
  inherited Destroy;
end;

procedure TXplSetVariable.SerializeToStream(pStream: TStream);
begin
  WriteIdentByte(pStream);
  pStream.WriteAnsiString(fName);
  if (fValue = nil) then
    fValue := TXplValue.Create;
  fValue.SerializeToStream(pStream)
end;

{ TXplMessage }

procedure TXplMessage.SerializeToStream(pStream: TStream);
begin

end;

{ TXplDrawText }

constructor TXplDrawText.Create(pText: String);
begin
  fText:=pText;
  fTimeInSec:=0;
  fPosition:=0.3;
end;

constructor TXplDrawText.Create(pStream: TStream);
begin
  fText := pStream.ReadAnsiString;
  pStream.Read(fPosition, SizeOf(fPosition));
  pStream.Read(fTimeInSec, SizeOf(TimeInSec));
end;

procedure TXplDrawText.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(HDMC_SHOW_TEXT);
  pStream.WriteAnsiString(fText);
  pStream.Write(fPosition, SizeOf(fPosition));
  pStream.Write(fTimeInSec, SizeOf(fTimeInSec));
end;

end.

