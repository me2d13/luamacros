unit uXplMessages;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  TXplVarType = (vtNull, vtInteger, vtDouble, vtString);

  PXplValue = ^TXplValueRec;
  TXplValueRec = packed record
    case VarType: TXplVarType of
    vtInteger: (intData : Int64);
    vtDouble: (doubleData : Double);
    vtString: (stringData: String[255]);
  end;

  TXplVariableWithValueRec = packed record
    Name: String[255];
    Value: TXplValueRec;
    Index: Int64;
  end;

  PXplGetVariableRequestRec = ^TXplGetVariableRequestRec;
  TXplGetVariableRequestRec = packed record
    Name: String[255];
    Index: Int64;
  end;


  TXplIncVariableRec = packed record
    SetVariableData: TXplVariableWithValueRec;
    HasLimit: Boolean;
    Limit: Double;
    UseOverflow: Boolean;
    OverflowBase: Double;
  end;


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

  { TXplValue }

  TXplValue = class
  private
    fValue: TXplValueRec;
    function GetDoubleValue: Double;
    function GetIntValue: Int64;
    function GetStringValue: String;
    function GetType: TXplVarType;
    procedure SetDoubleValue(AValue: Double);
    procedure SetIntValue(AValue: Int64);
    procedure SetStringValue(AValue: String);
  public
    constructor Create;overload;
    constructor Create(pIntValue: Integer);overload;
    constructor Create(pDoubleValue: Double);overload;
    constructor Create(pStringValue: String);overload;
    constructor Create(pStream: TStream);overload;
    constructor Create(pValue: TXplValueRec);overload;
    procedure SerializeToStream(pStream: TStream);virtual;
    procedure MakeDouble;
    procedure MakeInt;
    procedure MakeString;
    function ToString: ansistring;override;
    function Equals(pVar: PXplValue): boolean;
    function EqualsWithDelta(pVar: PXplValue; pDelta:Int64): boolean;
    function SameDouble(pVal1, pVal2, pEpsilon: double): boolean;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property DoubleValue: Double read GetDoubleValue write SetDoubleValue;
    property StringValue: String read GetStringValue write SetStringValue;
    property ValueType: TXplVarType read GetType;
    property Value: TXplValueRec read fValue;
  end;

  { TXplSetVariable }

  TXplSetVariable = class (TXplMessage)
  protected
    fName: String;
    fValue: TXplValue;
    fIndex: Int64;
    procedure WriteIdentByte(pStream: TStream);virtual;
  public
    constructor Create(pName: String; pValue: TXplValue; pIndex: Integer);overload;
    constructor Create(pStream: TStream);overload;
    destructor Destroy;override;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property Value: TXplValue read fValue write fValue;
    property Index: Int64 read fIndex write fIndex;
  end;

  { TXplGetVariable }

  TXplGetVariable = class (TXplMessage)
  private
    fName: String;
    fId: Int64;
    fIndex: Int64;
  public
    constructor Create(pName: String; pId: Int64; pIndex: Integer);overload;
    constructor Create(pStream: TStream);overload;
    destructor Destroy;override;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property Id: Int64 read fId write fId;
    property Index: Int64 read fIndex write fIndex;
  end;

  { TXplVariableValue - answer to GetValue}

  TXplVariableValue = class (TXplSetVariable)
  protected
    fId: Int64;
    fChangeCount: Int64; // how many times variable was changed between 2 messages
    procedure WriteIdentByte(pStream: TStream);override;
  public
    constructor Create(pName: String; pValue: TXplValue; pId: Int64);overload;
    constructor Create(pName: String; pValue: TXplValue; pId: Int64; pChangeCount: Int64);overload;
    constructor Create(pName: String; pValue: TXplValue; pId: Int64; pChangeCount: Int64; pIndex: Int64);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    function ToString: ansistring;override;
    property Id: Int64 read fId write fId;
    property ChangeCount: Int64 read fChangeCount write fChangeCount;
  end;

  { TXplIncVariable }

  TXplIncVariable = class (TXplSetVariable)
  protected
    fHasLimit: Boolean;
    fLimit: Double;
    fUseOverflow: Boolean;
    fOverflowBase: Double;
    procedure WriteIdentByte(pStream: TStream);override;
  public
    constructor Create(pName: String; pValue: TXplValue);overload;
    constructor Create(pName: String; pValue: TXplValue; pLimit: Double);overload;
    constructor Create(pName: String; pValue: TXplValue; pLimit: Double; pOverflowBase: Double);overload;
    constructor Create(pStream: TStream);overload;
    constructor Create(pData: TXplIncVariableRec);overload;
    procedure SerializeToStream(pStream: TStream);override;
    function ToString: ansistring;override;
    property HasLimit: Boolean read fHasLimit write fHasLimit;
    property UseOverflow: Boolean read fUseOverflow write fUseOverflow;
    property Limit: Double read fLimit write fLimit;
    property OverflowBase: Double read fOverflowBase write fOverflowBase;
  end;

  { TXplReconnectToServer }

  TXplReconnectToServer = class (TXplMessage)
  public
    procedure SerializeToStream(pStream: TStream);override;
  end;

  { TXplCallWithName }

  TXplCallWithName = class (TXplMessage)
  protected
    fName: String;
    procedure WriteIdentByte(pStream: TStream);virtual; abstract;
  public
    constructor Create(pName: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
  end;

  { TXplExecuteCommand }

  TXplExecuteCommand = class (TXplCallWithName)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplExecuteCommandBegin }

  TXplExecuteCommandBegin = class (TXplCallWithName)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplExecuteCommandEnd }

  TXplExecuteCommandEnd = class (TXplCallWithName)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplVariableCallback }

  TXplVariableCallback = class (TXplMessage)
  private
    fName: String;
    fIntervalMs: Int64;
    fDelta: Int64;
    fId: Int64;
  public
    constructor Create(pName: String; pIntervalMs: Int64; pDelta: Int64; pId: Int64);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Name: String read fName write fName;
    property IntervalMs: Int64 read fIntervalMs write fIntervalMs;
    property Delta: Int64 read fDelta write fDelta;
    property Id: Int64 read fId write fId;
  end;

  { TXplUnhookVariable }

  TXplUnhookVariable = class (TXplCallWithName)
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  end;

  { TXplLogCommand }

  TXplLogCommand = class (TXplCallWithName)
  private
    fValue: String;
  protected
    procedure WriteIdentByte(pStream: TStream);override;
  public
    constructor Create(pName: String; pValue: String);overload;
    constructor Create(pStream: TStream);overload;
    procedure SerializeToStream(pStream: TStream);override;
    property Value: String read fValue write fValue;
  end;


implementation

uses
  uXplCommon, math;

function StrToFloatWithDecimalPoint(const Value: String): Double;
var
  myFormatSettings: TFormatSettings;
begin
  //GetLocaleFormatSettings(GetThreadLocale, myFormatSettings);
  DecimalSeparator := '.';
  Result := StrToFloat(Value);
end;

{ TXplIncVariable }

procedure TXplIncVariable.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_INC_VARIABLE);
end;

constructor TXplIncVariable.Create(pName: String; pValue: TXplValue);
begin
  Create(pName, pValue, 0, 0);
  fUseOverflow:=False;
  fHasLimit:=False;
end;

constructor TXplIncVariable.Create(pName: String; pValue: TXplValue;
  pLimit: Double);
begin
  Create(pName, pValue, pLimit, 0);
  fUseOverflow:=False;
end;

constructor TXplIncVariable.Create(pName: String; pValue: TXplValue;
  pLimit: Double; pOverflowBase: Double);
begin
  inherited Create(pName, pValue, NO_INDEX);
  fLimit:=pLimit;
  fHasLimit:=True;
  fOverflowBase:=pOverflowBase;
  fUseOverflow:=True;
end;

constructor TXplIncVariable.Create(pStream: TStream);
begin
  inherited;
  pStream.Read(fHasLimit, SizeOf(fHasLimit));
  pStream.Read(fLimit, SizeOf(fLimit));
  pStream.Read(fUseOverflow, SizeOf(fUseOverflow));
  pStream.Read(fOverflowBase, SizeOf(fOverflowBase));
end;

constructor TXplIncVariable.Create(pData: TXplIncVariableRec);
begin
  fName:=pData.SetVariableData.Name;
  fValue := TXplValue.Create(pData.SetVariableData.Value);
  fIndex:=pData.SetVariableData.Index;
  fHasLimit:=pData.HasLimit;
  fLimit:=pData.Limit;
  fUseOverflow:=pData.UseOverflow;
  fOverflowBase:=pData.OverflowBase;
end;

procedure TXplIncVariable.SerializeToStream(pStream: TStream);
begin
  inherited SerializeToStream(pStream);
  pStream.Write(fHasLimit, SizeOf(fHasLimit));
  pStream.Write(fLimit, SizeOf(fLimit));
  pStream.Write(fUseOverflow, SizeOf(fUseOverflow));
  pStream.Write(fOverflowBase, SizeOf(fOverflowBase));
end;

function TXplIncVariable.ToString: ansistring;
begin
  Result := Format('name %s', [fName]);
  if (fIndex <> NO_INDEX) then
    Result := Result + Format('[%d]', [fIndex]);
  Result := Result + ' increased by ';
  if (fValue = nil) then
    Result := Result + 'nil'
  else
    Result := Result + fValue.ToString;
  if (fHasLimit) then
    Result := Result + Format(' limit %f', [fLimit]);
  if (fUseOverflow) then
    Result := Result + Format(' on overflow start from %f', [fOverflowBase]);
end;

{ TXplLogCommand }

procedure TXplLogCommand.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_LOG_COMMAND);
end;

constructor TXplLogCommand.Create(pName: String; pValue: String);
begin
  inherited Create(pName);
  fValue:=pValue;
end;

constructor TXplLogCommand.Create(pStream: TStream);
begin
  inherited;
  fValue:=pStream.ReadAnsiString;
end;

procedure TXplLogCommand.SerializeToStream(pStream: TStream);
begin
  inherited SerializeToStream(pStream);
  pStream.WriteAnsiString(fValue);
end;

{ TXplUnhookVariable }

procedure TXplUnhookVariable.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_UNHOOK_VAR);
end;

{ TXplExecuteCommand }

procedure TXplExecuteCommand.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_EXEC_COMMAND);
end;

{ TXplVariableCallback }

constructor TXplVariableCallback.Create(pName: String; pIntervalMs: Int64; pDelta: Int64; pId: Int64);
begin
  fName:=pName;
  fIntervalMs:=pIntervalMs;
  fId := pId;
  fDelta:=pDelta;
end;

constructor TXplVariableCallback.Create(pStream: TStream);
begin
  fName:=pStream.ReadAnsiString;
  pStream.Read(fId, SizeOf(fId));
  pStream.Read(fIntervalMs, SizeOf(fIntervalMs));
  pStream.Read(fDelta, SizeOf(fDelta));
end;

procedure TXplVariableCallback.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(HDMC_VAR_CALLBACK);
  pStream.WriteAnsiString(fName);
  pStream.Write(fId, SizeOf(fId));
  pStream.Write(fIntervalMs, SizeOf(fIntervalMs));
  pStream.Write(fDelta, SizeOf(fDelta));
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

{ TXplCallWithName }

constructor TXplCallWithName.Create(pName: String);
begin
  fName:=pName;
end;

constructor TXplCallWithName.Create(pStream: TStream);
begin
  fName:=pStream.ReadAnsiString;
end;

procedure TXplCallWithName.SerializeToStream(pStream: TStream);
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
  inherited Create(pName, pValue, NO_INDEX);
  fId:=pId;
end;

constructor TXplVariableValue.Create(pName: String; pValue: TXplValue;
  pId: Int64; pChangeCount: Int64);
begin
  Create(pName, pValue, pId);
  fChangeCount:=pChangeCount;
end;

constructor TXplVariableValue.Create(pName: String; pValue: TXplValue;
  pId: Int64; pChangeCount: Int64; pIndex: Int64);
begin
  inherited Create(pName, pValue, NO_INDEX);
  fId:=pId;
  fChangeCount:=pChangeCount;
end;

constructor TXplVariableValue.Create(pStream: TStream);
begin
  inherited;
  pStream.Read(fId, SizeOf(fId));
  pStream.Read(fChangeCount, SizeOf(fChangeCount));
end;

procedure TXplVariableValue.SerializeToStream(pStream: TStream);
begin
  inherited SerializeToStream(pStream);
  pStream.Write(fId, SizeOf(fId));
  pStream.Write(fChangeCount, SizeOf(fChangeCount));
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

constructor TXplGetVariable.Create(pName: String; pId: Int64; pIndex: Integer);
begin
  fName:=pName;
  fId:=pId;
  fIndex:=pIndex;
end;

constructor TXplGetVariable.Create(pStream: TStream);
begin
  fName := pStream.ReadAnsiString;
  pStream.Read(fId, SizeOf(fId));
  pStream.Read(fIndex, SizeOf(fIndex));
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
  pStream.Write(fIndex, SizeOf(fIndex));
end;

{ TXplValue }

procedure TXplValue.SetDoubleValue(AValue: Double);
begin
  fValue.VarType:=vtDouble;
  fValue.doubleData:=AValue;
end;

function TXplValue.GetIntValue: Int64;
begin
  Result := fValue.intData;
end;

function TXplValue.GetStringValue: String;
begin
  Result := fValue.stringData;
end;

function TXplValue.GetType: TXplVarType;
begin
  Result := fValue.VarType;
end;

function TXplValue.GetDoubleValue: Double;
begin
  Result := fValue.doubleData;
end;

procedure TXplValue.SetIntValue(AValue: Int64);
begin
  fValue.VarType:=vtInteger;
  fValue.intData:=AValue;
end;

procedure TXplValue.SetStringValue(AValue: String);
begin
  fValue.VarType:=vtString;
  fValue.stringData:=AValue;
end;

constructor TXplValue.Create;
begin
  fValue.VarType:=vtNull;
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
  fValue.VarType:=TXplVarType(pStream.ReadByte);
  case fValue.VarType of
    vtInteger: pStream.Read(fValue.intData, SizeOf(fValue.intData));
    vtDouble: pStream.Read(fValue.doubleData, SizeOf(fValue.doubleData));
    vtString: fValue.stringData := pStream.ReadAnsiString;
  end;
end;

constructor TXplValue.Create(pValue: TXplValueRec);
begin
  fValue := pValue;
end;

procedure TXplValue.SerializeToStream(pStream: TStream);
begin
  pStream.WriteByte(Ord(fValue.VarType));
  case fValue.VarType of
    vtInteger: pStream.Write(fValue.intData, SizeOf(fValue.intData));
    vtDouble: pStream.Write(fValue.doubleData, SizeOf(fValue.doubleData));
    vtString: pStream.WriteAnsiString(fValue.stringData);
  end;
end;

procedure TXplValue.MakeDouble;
begin
  if (fValue.VarType = vtDouble) then
    exit;
  case fValue.VarType of
    vtInteger: SetDoubleValue(fValue.intData);
    vtString: SetDoubleValue(StrToFloatWithDecimalPoint(fValue.stringData));
    else SetDoubleValue(0);
  end;
end;

procedure TXplValue.MakeInt;
begin
  if (fValue.VarType = vtInteger) then
    exit;
  case fValue.VarType of
    vtDouble: SetIntValue(trunc(fValue.doubleData));
    vtString: SetIntValue(StrToInt(fValue.stringData));
    else SetIntValue(0);
  end;
end;

procedure TXplValue.MakeString;
begin
  if (fValue.VarType = vtString) then
    exit;
  case fValue.VarType of
    vtDouble: SetStringValue(FloatToStr(fValue.doubleData));
    vtInteger: SetStringValue(IntToStr(fValue.intData));
    else SetStringValue('');
  end;
end;

function TXplValue.ToString: ansistring;
begin
  Result := 'N/A';
  case fValue.VarType of
    vtNull: Result := 'null';
    vtString: Result := '[string] "' + fValue.stringData + '"';
    vtInteger: Result := '[int] ' + IntToStr(fValue.intData);
    vtDouble: Result := '[double] ' + FloatToStr(fValue.doubleData);
  end;
end;

function TXplValue.Equals(pVar: PXplValue): boolean;
begin
  Result := False;
  if (pVar = nil) then exit;
  if (fValue.VarType <> pVar^.VarType) then exit;
  Result :=
    ((fValue.VarType = vtInteger) and (fValue.intData = pVar^.intData)) or
    ((fValue.VarType = vtDouble) and (SameDouble(fValue.doubleData, pVar^.doubleData, 0))) or
    ((fValue.VarType = vtString) and (fValue.stringData = pVar^.stringData)) or
    ((fValue.VarType = vtNull));
end;

function TXplValue.EqualsWithDelta(pVar: PXplValue; pDelta: Int64): boolean;
begin
  Result := False;
  if (pVar = nil) then exit;
  if (fValue.VarType <> pVar^.VarType) then exit;
  Result :=
    ((fValue.VarType = vtInteger) and (Abs(fValue.intData - pVar^.intData) < pDelta)) or
    ((fValue.VarType = vtDouble) and (SameDouble(fValue.doubleData, pVar^.doubleData, pDelta))) or
    ((fValue.VarType = vtString) and (fValue.stringData = pVar^.stringData)) or
    ((fValue.VarType = vtNull));
end;

function TXplValue.SameDouble(pVal1, pVal2, pEpsilon: double): boolean;
begin
  Result := (IsNan(pVal1) and IsNan(pVal2))
         or (IsInfinite(pVal1) and IsInfinite(pVal2))
         or (SameValue(pVal1, pVal2, pEpsilon));
end;

{ TXplSetVariable }

procedure TXplSetVariable.WriteIdentByte(pStream: TStream);
begin
  pStream.WriteByte(HDMC_SET_VAR);
end;

constructor TXplSetVariable.Create(pName: String; pValue: TXplValue; pIndex: Integer);
begin
  fName:=pName;
  fValue := pValue;
  fIndex:=pIndex;
end;

constructor TXplSetVariable.Create(pStream: TStream);
begin
  fName := pStream.ReadAnsiString;
  fValue := TXplValue.Create(pStream);
  pStream.Read(fIndex, SizeOf(fIndex));
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
  fValue.SerializeToStream(pStream);
  pStream.Write(fIndex, SizeOf(fIndex));
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

