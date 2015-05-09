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
    fIntValue: Integer;
    fDoubleValue: Double;
    fStringValue: String;
    procedure SetDoubleValue(AValue: Double);
    procedure SetIntValue(AValue: Integer);
    procedure SetStringValue(AValue: String);
  public
    constructor Create;overload;
    constructor Create(pIntValue: Integer);overload;
    constructor Create(pDoubleValue: Double);overload;
    constructor Create(pStringValue: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);
    procedure MakeDouble;
    procedure MakeInt;
    procedure MakeString;
    function ToString: ansistring;override;
    property IntValue: Integer read fIntValue write SetIntValue;
    property DoubleValue: Double read fDoubleValue write SetDoubleValue;
    property StringValue: String read fStringValue write SetStringValue;
    property ValueType: TXplVarType read fType;
  end;

  { TXplSetVariable }

  TXplSetVariable = class (TXplMessage)
  private
    fName: String;
    fValue: TXplValue;
  public
    constructor Create(pName: String; pValue: TXplValue);overload;
    constructor Create(pStream: TStream);overload;
    destructor Destroy;override;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property Value: TXplValue read fValue write fValue;
  end;


implementation

uses
  uXplCommon;

{ TXplValue }

procedure TXplValue.SetDoubleValue(AValue: Double);
begin
  fDoubleValue:=AValue;
  fType:=vtDouble;
end;

procedure TXplValue.SetIntValue(AValue: Integer);
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

{ TXplSetVariable }

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
  pStream.WriteByte(HDMC_SET_VAR);
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

