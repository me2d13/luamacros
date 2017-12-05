unit uXplControl;

interface

uses uXplCommon, Classes, uXplListener, uXplSender, uXplMessages, fgl, MemMap;

type

  TCallbackInfoList = TFPGList<TLmcVariableCallbackInfo>;

  { TXPLcontrol }

  TXPLcontrol = class
  private
    fCallbacks: TCallbackInfoList;
    fLmc2XplMem: TMemMap;
    fXpl2LmcMem: TMemMap;
    fXplMem: PXpl2LmcSharedMem;
    fLmcMem: PLmc2XplSharedMem;
    fRequestSequence: Int64;
    procedure DebugLog(Value: String);
    procedure DebugLogFmt(pFormat:String; pArgs: array of const);
    function getCommandSlotAndInitHeader: PLmcCommandRec;
    function GetVariableValueFromXplSharedMemory(pName: String; pIndex: Integer): TXplValue;
    procedure CheckVaribleCallbacks;
    function IsXplConnected: Boolean;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure Init;
    procedure Tick;
    function GetXplVariable(pName: String): TXplValue; overload;
    function GetXplVariable(pName: String; pIndex: Integer): TXplValue; overload;
    procedure SetXplVariable(pName: String; pValue: TXplValue); overload;
    procedure SetXplVariable(pName: String; pValue: TXplValue; pIndex: Integer); overload;
    procedure IncVariable(pData: TXplIncVariableRec);
    procedure ExecuteCommand(pCmdName: String);
    procedure ExecuteCommandBegin(pCmdName: String);
    procedure ExecuteCommandEnd(pCmdName: String);
    procedure DrawText(pText: String; pPos: Single = 0; pSec: Integer = 5);
    procedure SetVariableHook(pVarName: String; pHandlerRef: Integer; pIntervalMs: Integer; pDelta: Integer);
    procedure UnhookVariable(pVarName: String);
    procedure Reset;
    procedure LogCommand(pPar1: String; pPar2: String);
    property Callbacks: TCallbackInfoList read fCallbacks;
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
  fRequestSequence := 1;
  fCallbacks := TCallbackInfoList.Create;
  fXpl2LmcMem := TMemMap.Create(cXpl2LmcMemName, SizeOf(TXpl2LmcSharedMem), True);
  fXplMem:=fXpl2LmcMem.Memory;
  fLmc2XplMem := TMemMap.Create(cLmc2XplMemName, SizeOf(TLmc2XplSharedMem), True);
  fLmcMem:=fLmc2XplMem.Memory;
end;

procedure TXPLcontrol.DebugLog(Value: String);
begin
  if Glb <> nil then
    Glb.DebugLog(Value, cLoggerXpl);
end;

procedure TXPLcontrol.DebugLogFmt(pFormat: String; pArgs: array of const);
begin
  if Glb <> nil then
    Glb.DebugLogFmt(pFormat, pArgs, cLoggerXpl);
end;

destructor TXPLcontrol.Destroy;
var I: Integer;
begin
  fCallbacks.Free;
  inherited;
end;

procedure TXPLcontrol.Init;
var
  lCom: PLmcCommandRec;
begin
  fRequestSequence := fXplMem.LastProcessedId + 1;   // ususally 0+1, but if XPL
  // is running and LMC was restarted, the sequence continues (otherwise XPL would
  // ignore requests 1,2,3... as processed
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_LMC_STARTED;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog('Sending LMC started command to reset XPL plugin structures');
  end;
end;

procedure TXPLcontrol.Tick;
begin
  if IsXplConnected then
    CheckVaribleCallbacks;
end;

function TXPLcontrol.GetXplVariable(pName: String): TXplValue;
begin
  Result := GetXplVariable(pName, NO_INDEX);
end;

procedure TXPLcontrol.DrawText(pText: String; pPos: Single; pSec: Integer);
var
  lCom: PLmcCommandRec;
begin
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_DRAW_TEXT;
    lCom^.TextData.Position:=pPos;
    lCom^.TextData.TimeInSec:=pSec;
    lCom^.TextData.Value:=pText;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending DrawText command for text %s at pos %f.', [pText, pPos]));
    //DebugLogFmt('Size of TLmc2XplSharedMem is %d', [SizeOf(TLmc2XplSharedMem)]);
  end;
end;

procedure TXPLcontrol.SetVariableHook(pVarName: String; pHandlerRef: Integer;
  pIntervalMs: Integer; pDelta: Integer);
var
  lCbInfo: TLmcVariableCallbackInfo;
begin
  lCbInfo := TLmcVariableCallbackInfo.Create;
  lCbInfo.Interval:=pIntervalMs;
  lCbInfo.Delta:=pDelta;
  lCbInfo.Name:=pVarName;
  lCbInfo.LuaHandlerRef:=pHandlerRef;
  fCallbacks.Add(lCbInfo);
  Glb.DebugLog(Format('Registered variable callback for %s with interval %d and delta %d',
    [pVarName, pIntervalMs, pDelta]), cLoggerXpl);
end;

procedure TXPLcontrol.UnhookVariable(pVarName: String);
var
  I:Integer;
  lXplObj: TXplUnhookVariable;
begin
  // remove from XPL plugin
  lXplObj := TXplUnhookVariable.Create(pVarName);
  //fXplSender.SendMessage(lXplObj);
  // remove from my records
  i := 0;
  while i < fCallbacks.Count do
  begin
    if UpperCase(pVarName) = UpperCase(fCallbacks[I].Name) then
    begin
      Glb.DebugLog(Format('Removing registered variable %s with id %d and interval %d',
        [fCallbacks[I].Name, fCallbacks[I].Interval]), cLoggerXpl);
      fCallbacks.Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure TXPLcontrol.Reset;
var
  lCom: PLmcCommandRec;
begin
  fCallbacks.Clear;
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_XPL_COMMAND;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog('Sending XPL reset command.');
  end;
end;

procedure TXPLcontrol.LogCommand(pPar1: String; pPar2: String);
var
  lXplObj: TXplCallWithName;
begin
  lXplObj := TXplLogCommand.Create(pPar1, pPar2);
  Glb.DebugLog(Format('XPL plugin log command %s value %s', [pPar1, pPar2]), cLoggerXpl);
  //fXplSender.SendMessage(lXplObj);
  lXplObj.Free;
end;

function TXPLcontrol.getCommandSlotAndInitHeader: PLmcCommandRec;
var
  lIndex: Integer;
  lCommand: PLmcCommandRec;
begin
  lCommand:=nil;
  for lIndex := Low(fLmcMem^.Commands) to High(fLmcMem^.Commands) do
  begin
    if fLmcMem^.Commands[lIndex].Header.Status = LMC_STATUS_FREE then
    begin
      lCommand:=@fLmcMem^.Commands[lIndex];
      break;
    end;
    if (fLmcMem^.Commands[lIndex].Header.Status = LMC_STATUS_READY) and
      (fXplMem.LastProcessedId > fLmcMem^.Commands[lIndex].Header.Id) then
    begin
      lCommand:=@fLmcMem^.Commands[lIndex];
      lCommand.Header.Status:=LMC_STATUS_FREE;
      break;
    end;
  end;
  if (lCommand = nil) then
  begin
    DebugLog(Format('No free slots (out of %d) for XPL command. Xplane probably not running.', [High(fLmcMem^.Commands) - Low(fLmcMem^.Commands)]));
    Result := nil;
  end else begin
    Result := lCommand;
    Result^.Header.Status:=LMC_STATUS_WRITING;
    Result^.Header.Id:=fRequestSequence;
    Result^.Header.TimeStamp:=Glb.UnixTimestampMs;
    DebugLog(Format('Initiated command slot %d with request id %d at %d.', [lIndex, fRequestSequence, Result^.Header.TimeStamp]));
    Inc(fRequestSequence);
    DebugLog(Format('XPL memory: last id %d, updated %d.', [fXplMem^.LastProcessedId, fXplMem^.UpdateTimeStamp]));
  end;
end;

function TXPLcontrol.GetVariableValueFromXplSharedMemory(pName: String;
  pIndex: Integer): TXplValue;
var
  lIndex: Integer;
  lFeedback: TXplFeedbackRec;
begin
  for lIndex := Low(fXplMem^.Values) to High(fXplMem^.Values) do
  begin
    if (fXplMem^.Values[lIndex].Header.Status = LMC_STATUS_READY)
      and (fXplMem^.Values[lIndex].Value.Index = pIndex)
      and (fXplMem^.Values[lIndex].Header.TimeStamp + cAcceptableXplAnswerDelayInMs >= Glb.UnixTimestampMs)
      and (fXplMem^.Values[lIndex].Value.Name = pName) then
    begin
      Result := TXplValue.Create(fXplMem^.Values[lIndex].Value.Value);
      exit;
    end;
  end;
  Result := nil;
end;

procedure TXPLcontrol.CheckVaribleCallbacks;
var
  I: Integer;
  lCallbackInfo: TLmcVariableCallbackInfo;
  lValue: TXplValue;
  lChanged: boolean;
  lSend: boolean;
  lNow: Int64;
begin
  for I := 0 to Pred(fCallbacks.Count) do
  begin
    lCallbackInfo := fCallbacks[I];
    // get value
    lValue := GetXplVariable(lCallbackInfo.Name);
    if (lValue = nil) then
      Continue; // can not find out variable value
    // compare with last from info

    lChanged:= (lCallbackInfo.LastValue.VarType = vtNull); // first assignment
    if (not lChanged) then
    begin
      if (lCallbackInfo.Delta = 0) then
      begin
        lChanged := not lValue.Equals(@lCallbackInfo.LastValue) // different value
      end
      else
      begin
        lChanged := not lValue.EqualsWithDelta(@lCallbackInfo.LastValue, lCallbackInfo.Delta); // different value
        {$ifdef LMC_XPL_VERY_VERBOSE_DEBUG}
        if (lChanged) then
          Glb.DebugLogFmt('Different values with delta %d, old %s, new %s',
            [lCallbackInfo.Delta, XplValueRecToString(lCallbackInfo.LastValue), lValue.ToString], cLoggerXpl);
        {$endif}
      end;
    end;
    if (lChanged) or (lCallbackInfo.ChangeCount > 0) then
    begin
      if lChanged then
      begin
        lCallbackInfo.LastValue := lValue.Value;
        Inc(lCallbackInfo.ChangeCount);
      end;
      // should we send the change? Calculate interval
      lNow := Glb.UnixTimestampMs;
      lSend := (lNow - lCallbackInfo.LastTriggerTs) >= lCallbackInfo.Interval;
      if (lSend) then
      begin
        Glb.DebugLog(Format('Calling Lua function %d on var %s change to %s and change count %d.',
              [lCallbackInfo.LuaHandlerRef, lCallbackInfo.Name, lValue.ToString, lCallbackInfo.ChangeCount]), cLoggerXpl);
        Glb.LuaEngine.CallFunctionByRef(lCallbackInfo.LuaHandlerRef, @lValue.Value, lCallbackInfo.ChangeCount);
        lCallbackInfo.ChangeCount:=0;
        lCallbackInfo.LastTriggerTs:=lNow;
        Inc(lCallbackInfo.ActivationCount);
      end;
    end
    else
      lValue.Free;
  end;
end;

function TXPLcontrol.IsXplConnected: Boolean;
begin
  Result := (Glb.UnixTimestampMs - fXplMem.UpdateTimeStamp) < cMaxAcceptableRequestDelayMs;
end;

function TXPLcontrol.GetXplVariable(pName: String; pIndex: Integer): TXplValue;
var
  lCom: PLmcCommandRec;
  lTickCount: ULONGLONG;
begin
  Glb.StatsService.ReadXplVar(pName);
  Result := GetVariableValueFromXplSharedMemory(pName, pIndex);
  if (Result <> nil) then
    exit;
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_GET_VARIABLE;
    lCom^.GetVariableData.Name:=pName;
    lCom^.GetVariableData.Index:=pIndex;
    lCom^.Header.Status:=LMC_STATUS_READY;
    if (pIndex = NO_INDEX) then
      DebugLog(Format('Sending GetXplVar command for name %s.', [pName]))
    else
      DebugLog(Format('Sending GetXplVar command for name %s[%d].', [pName, pIndex]));
  end;
  lTickCount := GetTickCount64;
  while (lTickCount + cAcceptableXplAnswerDelayInMs > GetTickCount64) do
  begin
    Result := GetVariableValueFromXplSharedMemory(pName, pIndex);
    if (Result <> nil) then
      exit;
    Sleep(20);
  end;
  if (pIndex = NO_INDEX) then
    DebugLog(Format('No answer from XPL for GetXplVar command for name %s.', [pName]))
  else
    DebugLog(Format('No answer from XPL for GetXplVar command for name %s[%d].', [pName, pIndex]));
end;

procedure TXPLcontrol.SetXplVariable(pName: String; pValue: TXplValue);
begin
  SetXplVariable(pName, pValue, NO_INDEX);
end;

procedure TXPLcontrol.SetXplVariable(pName: String; pValue: TXplValue;
  pIndex: Integer);
var
  lCom: PLmcCommandRec;
begin
  Glb.StatsService.WriteXplVar(pName);
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_SET_VARIABLE;
    lCom^.SetVariableData.Name:=pName;
    lCom^.SetVariableData.Value := pValue.Value;
    lCom^.SetVariableData.Index:=pIndex;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending SetVar command for variable %s index %d.', [pName, pIndex]));
  end;
end;

procedure TXPLcontrol.IncVariable(pData: TXplIncVariableRec);
var
  lCom: PLmcCommandRec;
begin
  Glb.StatsService.WriteXplVar(pData.SetVariableData.Name);
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_INC_VARIABLE;
    lCom^.IncVariableData := pData;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending inc variable %s.', [pData.SetVariableData.Name]));
  end;
end;

procedure TXPLcontrol.ExecuteCommand(pCmdName: String);
var
  lCom: PLmcCommandRec;
begin
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_XPL_COMMAND;
    lCom^.CommandData.Name:=pCmdName;
    lCom^.CommandData.Mode:=XPL_COMMAND_EXECUTE;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending execute command %s.', [pCmdName]));
    Glb.StatsService.XplCommandExecuted(pCmdName);
  end;
end;

procedure TXPLcontrol.ExecuteCommandBegin(pCmdName: String);
var
  lCom: PLmcCommandRec;
begin
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_XPL_COMMAND;
    lCom^.CommandData.Name:=pCmdName;
    lCom^.CommandData.Mode:=XPL_COMMAND_START;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending begin execute command %s.', [pCmdName]));
    Glb.StatsService.XplCommandExecuted(pCmdName);
  end;
end;

procedure TXPLcontrol.ExecuteCommandEnd(pCmdName: String);
var
  lCom: PLmcCommandRec;
begin
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_XPL_COMMAND;
    lCom^.CommandData.Name:=pCmdName;
    lCom^.CommandData.Mode:=XPL_COMMAND_END;
    lCom^.Header.Status:=LMC_STATUS_READY;
    DebugLog(Format('Sending end execute command %s.', [pCmdName]));
    Glb.StatsService.XplCommandExecuted(pCmdName);
  end;
end;


end.
