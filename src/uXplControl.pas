unit uXplControl;

interface

uses MemMap, uXplCommon, Classes, uXplListener;

type

  { TXPLcontrol }

  TXPLcontrol = class
  private
    fMM: TMemMap;
    pBuffer: PXplComRecord;
    fVars: TStringList;
    fCommands: TStringList;
    fXplListener: TXplListener;
    procedure DebugLog(Value: String);
    function GetCustomXplVariable(pName: String; pIndex: Integer; isArray: Boolean): Variant;
    procedure SetCustomXplVariable(pName: String; pIndex: Integer; pValue: Variant; isArray: Boolean; pToggleCommand: Integer);
    procedure WaitForXplane(mSec: Integer = 500);
    procedure WaitForXplaneSlot(pSlot: Integer; mSec: Integer = 500);
    function GetFreeComSlot: Integer;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure Init;
    function IsXplaneReady: Boolean;
    function isXplaneConnected: Boolean;
    function GetXplVariable(pName: String): Variant;
    function GetXplArrayItem(pName: String; pIndex: Integer): Variant;
    procedure SetXplVariable(pName: String; pValue: Variant);
    procedure SetXplArrayItem(pName: String; pIndex: Integer; pValue: Variant);
    procedure ExecuteCommand(pCmdName: String; pMode: Byte = HDMC_EXEC_COMMAND);
    procedure ToggleDataRef(pName: String; pValues: String; pCommand: Integer);
    procedure DrawText(pText: String; pPos: Single = 0; pSec: Integer = 5);
  end;

  TXPLRefHolder = class
  private
    fData: Pointer8b;
  public
    property Data: Pointer8b read fData write fData;
  end;



implementation

uses SysUtils, Windows, Forms, XPLMDataAccess, Variants,
  uGlobals;

{ TXPLcontrol }

constructor TXPLcontrol.Create;
begin
  //lGlb.DebugLog('Xplane control created.', 'XPL');
  fVars := TStringList.Create;
  fVars.CaseSensitive := False;
  fCommands := TStringList.Create;
  fCommands.CaseSensitive := False;
  fXplListener := TXplListener.Create;
  fMM := TMemMap.Create(XPL_MEM_FILE, SizeOf(TXplComRecord));
end;

procedure TXPLcontrol.DebugLog(Value: String);
begin
  if Glb <> nil then
    Glb.DebugLog(Value, cLoggerXpl);
end;

destructor TXPLcontrol.Destroy;
var I: Integer;
begin
  for I := 0 to fVars.Count - 1 do
    fVars.Objects[I].Free;
  fVars.Free;
  for I := 0 to fCommands.Count - 1 do
    fCommands.Objects[I].Free;
  fXplListener.Free;
  fCommands.Free;
  fMM.Free;
  inherited;
end;

procedure TXPLcontrol.Init;
begin
  if fMM.Memory <> nil then
  begin
    pBuffer := fMM.Memory;
    //DebugLog(Format('pBuffer addr is %s.', [IntToStr(ULong(pBuffer))]));
    if pBuffer^.XplConnected > 0 then
      Glb.DebugLog('Xplane already connected to shared memory.', 'XPL')
    else
      Glb.DebugLog('Xplane not yet connected to shared memory.', 'XPL');
    pBuffer^.HdmConnected := 1;
    pBuffer^.ComSlots[0].HDMcommand := 0; // NOP, just ping
    pBuffer^.ComSlots[0].XplRequestFlag := 1;
    pBuffer^.XplRequestFlag := 1; // ask XPL for response
  end;
  fXplListener.Init;
  //lGlb.DebugLog(Format('Slot size: %d, mem size: %d', [SizeOf(TXplComSlot), SizeOf(TXplComRecord)]), 'XPL');
end;

procedure TXPLcontrol.ExecuteCommand(pCmdName: String; pMode: Byte = HDMC_EXEC_COMMAND);
var
  lCommandIndex: Integer;
  //lCommand: XPLMCommandRef;
  lCommand: Pointer8b;
  lUnregistered: Boolean;
  lSlot: Integer;
  lRef: TXPLRefHolder;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
  begin
    DebugLog('Can''t execute command, Xplane not conected.');
    exit;
  end;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t execute command, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  lCommandIndex := fCommands.IndexOf(pCmdName);
  if lCommandIndex > -1 then
  begin
    lCommand := TXPLRefHolder(fCommands.Objects[lCommandIndex]).Data;
    pBuffer^.ComSlots[lSlot].CommandRef := lCommand;
    lUnregistered := False;
  end
  else
  begin
    // find out first
    StrPLCopy(pBuffer^.ComSlots[lSlot].ValueName, pCmdName, High(pBuffer^.ComSlots[lSlot].ValueName));
    pBuffer^.ComSlots[lSlot].CommandRef := 0; // clear some old value
    //pSlot := @(pBuffer^.ComSlots[lSlot]);
    lUnregistered := True;
    //DebugLog('Looking for command ' + pCmdName + ' using slot ' + IntToStr(lSlot));
    //DebugLog('Slot dump, size of slot is ' + IntToStr(SizeOf(pBuffer^.ComSlots[lSlot])));
    //DebugLog(MemoryDump(Addr(pBuffer^.ComSlots[lSlot]), 1570));
  end;
  pBuffer^.ComSlots[lSlot].HDMcommand := pMode;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
  // if unregistered, wait for result and note down the address
  if lUnregistered then
  begin
    WaitForXplaneSlot(lSlot);
    if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
    begin
      lCommand := pBuffer^.ComSlots[lSlot].CommandRef;
      if lCommand = 0 then
        Glb.LogError(Format('Command %s not recognized by Xplane.', [pCmdName]), 'XPL')
      else
      begin
        lRef := TXPLRefHolder.Create;
        lRef.Data := lCommand;
        fCommands.AddObject(pCmdName, lRef);
        DebugLog(Format('Registered command %s as address %x.', [pCmdName, pBuffer^.ComSlots[lSlot].CommandRef]));
      end;
    end
    else
    begin
      DebugLog('No response from XPL');
    end;
  end;
end;

procedure TXPLcontrol.ToggleDataRef(pName: String; pValues: String;
  pCommand: Integer);
begin
  SetCustomXplVariable(pName, 0, pValues, False, pCommand);
end;

procedure TXPLcontrol.DrawText(pText: String; pPos: Single; pSec: Integer);
var
  lSlot: Integer;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t draw text, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;

  // find out first
  StrPCopy(pBuffer^.ComSlots[lSlot].StringBuffer, pText);
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_SHOW_TEXT;
  pBuffer^.ComSlots[lSlot].Value.floatData := pPos;
  pBuffer^.ComSlots[lSlot].Length := pSec;
  DebugLog(Format('Sending DrawText command for text %s at pos %f.', [pText, pPos]));
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
end;

function TXPLcontrol.GetCustomXplVariable(pName: String; pIndex: Integer;
  isArray: Boolean): Variant;
var
  lVarIndex: Integer;
  lVar: TXplVariable;
  lRes: String;
  lSlot: Integer;
begin
  Result := 0;
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t get variable, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  lVarIndex := fVars.IndexOf(pName);
  if lVarIndex > -1 then
  begin
    lVar := fVars.Objects[lVarIndex] as TXplVariable;
    pBuffer^.ComSlots[lSlot].DataRef := lVar.DataRef;
    pBuffer^.ComSlots[lSlot].DataType := lVar.DataType;
  end
  else
  begin
    // find out first
    StrPCopy(pBuffer^.ComSlots[lSlot].ValueName, pName);
    pBuffer^.ComSlots[lSlot].DataRef := 0;
  end;
  if isArray then
    pBuffer^.ComSlots[lSlot].Index := pIndex;
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_GET_VAR;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
  // wait for response
  WaitForXplaneSlot(lSlot);
  if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
  begin
    if pBuffer^.ComSlots[lSlot].DataType <> xplmType_Data then
    begin
      case pBuffer^.ComSlots[lSlot].DataType of
        xplmType_Float:  Result := pBuffer^.ComSlots[lSlot].Value.floatData;
        xplmType_Double: Result := pBuffer^.ComSlots[lSlot].Value.doubleData;
        xplmType_Int:    Result := pBuffer^.ComSlots[lSlot].Value.intData;
        xplmType_IntArray:   Result := pBuffer^.ComSlots[lSlot].Value.intData;
        xplmType_FloatArray: Result := pBuffer^.ComSlots[lSlot].Value.floatData;
      end;
    end
    else
    begin
      // string
      pBuffer^.ComSlots[lSlot].StringBuffer[XPL_MAX_STRING_SIZE -1] := #0; // for sure
      lRes := pBuffer^.ComSlots[lSlot].StringBuffer;
      DebugLog('Received string result ' + lRes);
      Result := lRes;
    end;
    if lVarIndex < 0 then
    begin
      // register returned variable ref
      if (pBuffer^.ComSlots[lSlot].DataRef = 0) then
        DebugLog('WARNING: no dataref received from Xplane for variable ' + pName + '.')
      else
      begin
        lVar := TXplVariable.Create;
        lVar.Name := pName;
        lVar.DataRef := pBuffer^.ComSlots[lSlot].DataRef;
        lVar.DataType := pBuffer^.ComSlots[lSlot].DataType;
        lVar.Writable := pBuffer^.ComSlots[lSlot].Writable;
        if lVar.IsArray then
          lVar.Length := pBuffer^.ComSlots[lSlot].Length
        else
          lVar.Length := 0;
        fVars.AddObject(pName, lVar);
        DebugLog(Format('Registered var %s at address %x.', [pName, pBuffer^.ComSlots[lSlot].DataRef]))
      end;
    end;
  end
  else
  begin
    DebugLog('Variable ' + pName + ' timed out - no response from Xplane.');
  end;
end;

function TXPLcontrol.GetFreeComSlot: Integer;
var I: Integer;
begin
  for I := 0 to COM_SLOTS_COUNT - 1 do
    if pBuffer^.ComSlots[I].XplRequestFlag = 0 then
    begin
      Result := I;
      exit;
    end;
  Result := -1;
end;

function TXPLcontrol.GetXplArrayItem(pName: String; pIndex: Integer): Variant;
begin
  Result := GetCustomXplVariable(pName, pIndex, True);
end;

function TXPLcontrol.GetXplVariable(pName: String): Variant;
begin
  Result := GetCustomXplVariable(pName, 0, False);
end;

function TXPLcontrol.isXplaneConnected: Boolean;
begin
  Result := (pBuffer <> nil) and (pBuffer^.XplConnected = 1);
end;

function TXPLcontrol.IsXplaneReady: Boolean;
begin
  Result := (pBuffer <> nil) and (pBuffer^.XplRequestFlag = 0);
end;

procedure TXPLcontrol.SetCustomXplVariable(pName: String; pIndex: Integer;
  pValue: Variant; isArray: Boolean; pToggleCommand: Integer);
var
lVarIndex: Integer;
lVar: TXplVariable;
lIsString: Boolean;
lStr: String;
lUnregistered: Boolean;
lSlot: Integer;
begin
if (fMM.Memory = nil) or (not isXplaneConnected) then
  exit;
lSlot := GetFreeComSlot;
if lSlot < 0 then
begin
  WaitForXplane;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    DebugLog('Can''t set variable '+pName+', Xplane not listening.');
    exit;
  end;
end;
if Glb.IsModuleLogged('XPL') then
  pBuffer^.Debug := True;
lIsString := VarType(pValue) = varOleStr;
lVarIndex := fVars.IndexOf(pName);
if lVarIndex > -1 then
begin
  lVar := fVars.Objects[lVarIndex] as TXplVariable;
  pBuffer^.ComSlots[lSlot].DataRef := lVar.DataRef;
  pBuffer^.ComSlots[lSlot].DataType := lVar.DataType;
  lIsString := pBuffer^.ComSlots[lSlot].DataType = xplmType_Data;
  lUnregistered := False;
end
else
begin
  // find out first
  StrPCopy(pBuffer^.ComSlots[lSlot].ValueName, pName);
  pBuffer^.ComSlots[lSlot].DataRef := 0;
  lUnregistered := True;
end;
if isArray then
  pBuffer^.ComSlots[lSlot].Index := pIndex;
lStr := pValue;
if lIsString then
begin
  StrLCopy(pBuffer^.ComSlots[lSlot].StringBuffer, PChar(lStr), XPL_MAX_STRING_SIZE);
  //pBuffer^.Length := Length(lStr); // keep always value from Xplane
  DebugLog('Setting string variable to ' + lStr);
end
else
  //pBuffer^.ComSlots[lSlot].Value := Variant2VariantBuffer(pValue);
  StrLCopy(pBuffer^.ComSlots[lSlot].ValueUntyped, PChar(lStr), 255);
if pToggleCommand > 0 then
  pBuffer^.ComSlots[lSlot].HDMcommand := pToggleCommand
else
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_SET_VAR;
pBuffer^.ComSlots[lSlot].XplRequestFlag := 1;
pBuffer^.XplRequestFlag := 1; // trigger xpl
// if unregistered, wait for result and note down the address
if lUnregistered then
begin
  WaitForXplaneSlot(lSlot);
  if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
  begin
    lVar := TXplVariable.Create;
    lVar.Name := pName;
    lVar.DataRef := pBuffer^.ComSlots[lSlot].DataRef;
    lVar.DataType := pBuffer^.ComSlots[lSlot].DataType;
    lVar.Writable := pBuffer^.ComSlots[lSlot].Writable;
    if lVar.IsArray or lVar.IsString then
      lVar.Length := pBuffer^.ComSlots[lSlot].Length
    else
      lVar.Length := 0;
    fVars.AddObject(pName, lVar);
    DebugLog(Format('Registered var %s at address %x.', [pName, pBuffer^.ComSlots[lSlot].DataRef]))
  end;
end;
end;

procedure TXPLcontrol.SetXplArrayItem(pName: String; pIndex: Integer;
  pValue: Variant);
begin
  SetCustomXplVariable(pName, pIndex, pValue, True, 0);
end;

procedure TXPLcontrol.SetXplVariable(pName: String; pValue: Variant);
begin
  SetCustomXplVariable(pName, 0, pValue, False, 0);
end;

procedure TXPLcontrol.WaitForXplane(mSec: Integer = 500);
var
  lEndMsec: Cardinal;
begin
  if fMM.Memory = nil then
    exit;
  lEndMsec := GetTickCount + mSec;
  while (GetTickCount < lEndMsec) and (pBuffer^.XplRequestFlag = 1) do
    //Application.ProcessMessages; -- only from main thread
    Sleep(10);
end;

procedure TXPLcontrol.WaitForXplaneSlot(pSlot: Integer; mSec: Integer);
var
  lEndMsec: Cardinal;
begin
  if fMM.Memory = nil then
    exit;
  lEndMsec := GetTickCount + mSec;
  while (GetTickCount < lEndMsec) and (pBuffer^.ComSlots[pSlot].XplRequestFlag = 1) do
    //Application.ProcessMessages;
    Sleep(10);
end;


end.
