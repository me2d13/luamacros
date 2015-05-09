unit uXplCommon;

interface

uses XPLMDataAccess, XPLMUtilities, classes;

const
  XPL_MEM_FILE = 'XPL_LUAMACROS_MEMORY_FILE';
  XPL_MAX_STRING_SIZE = 1024;

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

  COM_SLOTS_COUNT = 8;

  cXplToLmcPipeName = 'XplToLuaMacrosPipe';
  cLmcToXplPipeName = 'LuaMacrosToXplPipe';


type
  Pointer8b = Int64;

  PXplValue = ^TXplValueRec;
  TXplValueRec = packed record
    case Integer of
    0: (intData : Integer);
    1: (floatData : Single);
    2: (doubleData : Double);
  end;

  PXplComSlot = ^TXplComSlot;
  TXplComSlot = packed record
    XplRequestFlag: byte;
    HDMcommand: byte;
    //DataRef: XPLMDataRef;
    //CommandRef: XPLMCommandRef;
    DataRef: Pointer8b;
    CommandRef: Pointer8b;
    DataType: XPLMDataTypeID;
    Length: SmallInt;
    Index: Integer;
    Writable: Boolean;
    Value: TXplValueRec;
    ValueName: array[0..255] of char;
    ValueUntyped: array[0..255] of char;
    StringBuffer: array[0..XPL_MAX_STRING_SIZE-1] of char;
  end;

  PXplComRecord = ^TXplComRecord;
  TXplComRecord = packed record
    HdmConnected: byte;
    XplConnected: byte;
    XplRequestFlag: byte;
    ComSlots: array[0..COM_SLOTS_COUNT] of TXplComSlot;
    Debug: Boolean;
    Latitude: double;
    Longitude: double;
    Heading: double;
    Height: double;
    PosInterval: Integer;
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

  function Pointer2Pointer8b(Input: Pointer) : Pointer8b;
  function Pointer8b2Pointer(Input: Pointer8b) : Pointer;
  function MemoryDump(pAddr: Pointer; pSize: Integer) : String;


implementation

uses SysUtils;

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
