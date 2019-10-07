unit uXplCommon;

interface

uses XPLMDataAccess, classes, uXplMessages;

const
  HDMC_GET_VAR = 1;
  HDMC_SET_VAR = 2;
  HDMC_EXEC_COMMAND = 3;
  HDMC_COMMAND_BEGIN = 5;
  HDMC_COMMAND_END = 6;
  HDMC_SET_POSINTERVAL = 4;
  HDMC_TOGGLE_NEXT = 7;
  HDMC_TOGGLE_PREVIOUS = 8;
  HDMC_SWITCH_NEXT = 9;
  HDMC_SWITCH_PREVIOUS = 10;
  HDMC_SHOW_TEXT = 11;
  HDMC_VAR_RESPONSE = 12;
  HDMC_RECONNECT = 13;
  HDMC_VAR_CALLBACK = 14;
  HDMC_UNHOOK_VAR = 15;
  HDMC_LOG_COMMAND = 16;
  HDMC_INC_VARIABLE = 17;

  NO_INDEX = -1;

  cXplToLmcPipeName = 'XplToLuaMacrosPipe';
  cXplToLmcAsyncPipeName = 'XplToLuaMacrosAsyncPipe';
  cLmcToXplPipeName = 'LuaMacrosToXplPipe';

  cLogCommandLogFileName = 'LOG_FILE_NAME';
  cLogCommandProfile = 'PROFILE';

  cLmc2XplMemName = 'LuaMacrosToXplPlugin';
  cXpl2LmcMemName = 'XplPluginToLuaMacros';

  cMaxAcceptableRequestDelayMs = 5000; // to purge request queue and ignore such request by XPL

  cAcceptableXplAnswerDelayInMs = 500; // to accept variable value

  LOCK_NONE = 0;
  LOCK_VALUE_UPDATE = 1; // allow read
  LOCK_LIST_CHANGING = 2; // full lock

  LMC_STATUS_FREE = 0;
  LMC_STATUS_WRITING = 1;
  LMC_STATUS_READY = 2;

  XPL_COMMAND_EXECUTE = 1;
  XPL_COMMAND_START = 2;
  XPL_COMMAND_END = 3;

  LMC_COMMAND_LMC_STARTED = 1;
  LMC_COMMAND_DRAW_TEXT = 2;
  LMC_COMMAND_XPL_COMMAND = 3;
  LMC_COMMAND_SET_VARIABLE = 4;
  LMC_COMMAND_INC_VARIABLE = 5;
  LMC_COMMAND_GET_VARIABLE = 6;
  LMC_COMMAND_RESET = 7;


type
  Pointer8b = Int64;

  PComSlotRec = ^TComSlotRec;
  TComSlotRec = packed record
    Id: Int64;
    TimeStamp: Int64;
    Status: byte;
  end;


  TXplCommandRec = packed record
    Name: String[50];
    Mode: byte;
  end;

  TXplDrawTextRec = packed record
    Value: String[255];
    Position: Single;
    TimeInSec: Integer;
  end;

  PLmcCommandRec = ^TLmcCommandRec;
  TLmcCommandRec = packed record
    Header: TComSlotRec;
    case CommandType: byte of
    LMC_COMMAND_DRAW_TEXT: (TextData: TXplDrawTextRec);
    LMC_COMMAND_XPL_COMMAND: (CommandData: TXplCommandRec);
    LMC_COMMAND_SET_VARIABLE: (SetVariableData: TXplVariableWithValueRec);
    LMC_COMMAND_INC_VARIABLE: (IncVariableData: TXplIncVariableRec);
    LMC_COMMAND_GET_VARIABLE: (GetVariableData: TXplGetVariableRequestRec);
  end;

  TXplFeedbackRec = packed record
    Header: TComSlotRec;
    Value: TXplVariableWithValueRec
  end;

  PLmc2XplSharedMem = ^TLmc2XplSharedMem;
  TLmc2XplSharedMem = packed record
    Lock : byte;
    UpdateTimeStamp: Int64;
    Commands: array[0..100] of TLmcCommandRec;
  end;

  PXpl2LmcSharedMem = ^TXpl2LmcSharedMem;
  TXpl2LmcSharedMem = packed record
    Lock : byte;
    UpdateTimeStamp: Int64;
    LastProcessedId: Int64;
    Values: array[0..100] of TXplFeedbackRec;
  end;


  TXplVariable = class(TObject)
  public
    Name: String;
    DataType: XPLMDataTypeID;
    DataRef: XPLMDataRef;
    Writable: Boolean;
    Length: Integer;
    constructor Create;
    function IsArray: Boolean;
    function IsString: Boolean;
  end;

  { TXplVariableCallbackInfo }

  TXplVariableCallbackInfo = class(TObject)
  public
    Id: Int64;
    XplVariable: TXplVariable;
    LastCallback: Int64;
    LastValue: TXplValue;
    ChangeCount: Integer;
    Interval: Int64;
    Delta: Int64;
    destructor Destroy; Override;
  end;

  TLmcVariableCallbackInfo = class(TObject)
  public
    Name: String;
    Interval: Int64;
    Delta: Int64;
    LuaHandlerRef: Integer;
    LastValue: TXplValueRec;
    LastTriggerTs: Int64;
    ChangeCount: Integer;
    ActivationCount: Integer;
  end;


  function Pointer2Pointer8b(Input: Pointer) : Pointer8b;
  function Pointer8b2Pointer(Input: Pointer8b) : Pointer;
  function MemoryDump(pAddr: Pointer; pSize: Integer) : String;
  function UnixTimeStampCommonForXpl: Int64;


implementation

uses SysUtils, DateUtils;

function Pointer2Pointer8b(Input: Pointer) : Pointer8b;
begin
  {$IFDEF WIN64}
  Result := Pointer8b(Input);
  {$ELSE}
  Result := Pointer8b(Input);
  {$ENDIF}
end;

function Pointer8b2Pointer(Input: Pointer8b) : Pointer;
begin
  {$IFDEF WIN64}
  Result := Pointer(Input);
  {$ELSE}
  Result := Pointer(Input);
  {$ENDIF}
end;

const
  sLineBreak = AnsiString(#13#10);

function MemoryDump(pAddr: Pointer; pSize: Integer) : String;
var
  lP : PChar;
  i: Integer;
  lRow, lCol: Integer;
  lRes : String;
begin
  i := 0;
  lRes := '';
  lP := pAddr;
  while (i < pSize) do
  begin
    lRes := lRes + Format('%4d: ', [i]);
    for lCol := 1 to 10 do
    begin
      if (Ord(lP^) = 0) then
        lRes := lRes + Format('   [%x]', [Ord(lP^)])
      else
        lRes := lRes + Format(' %s [%x]', [lP^, Ord(lP^)]);
      Inc(i);
      Inc(lP);
      if (i = pSize) then
        break;
    end;
    lRes := lRes + sLineBreak;
  end;
  Result := lRes;
end;

function UnixTimeStampCommonForXpl: Int64;
begin
  Result := MilliSecondsBetween(Now, 25569); //25569 is TDateTime of Unix epoch 1.1.1970
end;

{ TXplVariableCallbackInfo }

destructor TXplVariableCallbackInfo.Destroy;
begin
  if LastValue <> nil then
    LastValue.Free;
  inherited Destroy;
end;

{ TXplVariable }

constructor TXplVariable.Create;
begin
  DataRef := 0;
end;

function TXplVariable.IsArray: Boolean;
begin
  Result := (DataType = xplmType_FloatArray) or (DataType = xplmType_IntArray);
end;

function TXplVariable.IsString: Boolean;
begin
  Result := (DataType = xplmType_Data);
end;

end.
