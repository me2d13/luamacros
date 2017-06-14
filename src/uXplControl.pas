unit uXplControl;

interface

uses uXplCommon, Classes, uXplListener, uXplSender, uXplMessages, fgl, MemMap;

type

  TCallbackInfoMap = TFPGMap<Int64,TLmcVariableCallbackInfo>;

  { TXPLcontrol }

  TXPLcontrol = class
  private
    fCallbackCount: Integer;
    fXplVariableValue: TXplVariableValue;
    fCallbacks: TCallbackInfoMap;
    fLmc2XplMem: TMemMap;
    fXpl2LmcMem: TMemMap;
    fXplMem: PXpl2LmcSharedMem;
    fLmcMem: PLmc2XplSharedMem;
    fRequestSequence: Int64;
    procedure DebugLog(Value: String);
    procedure DebugLogFmt(pFormat:String; pArgs: array of const);
    procedure OnXplSyncMessage(Sender: TObject);
    procedure OnXplAsyncMessage(Sender: TObject);
    function getCommandSlotAndInitHeader: PLmcCommandRec;
    function GetVariableValueFromXplSharedMemory(pName: String; pIndex: Integer): TXplValue;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure Init;
    function GetXplVariable(pName: String): TXplValue; overload;
    function GetXplVariable(pName: String; pIndex: Integer): TXplValue; overload;
    procedure SetXplVariable(pName: String; pValue: TXplValue); overload;
    procedure SetXplVariable(pName: String; pValue: TXplValue; pIndex: Integer); overload;
    procedure IncVariable(pData: TXplIncVariableRec);
    procedure ExecuteCommand(pCmdName: String);
    procedure ExecuteCommandBegin(pCmdName: String);
    procedure ExecuteCommandEnd(pCmdName: String);
    procedure DrawText(pText: String; pPos: Single = 0; pSec: Integer = 5);
    procedure XplVarProcessed;
    procedure SetVariableHook(pVarName: String; pHandlerRef: Integer; pIntervalMs: Integer; pDelta: Integer);
    procedure UnhookVariable(pVarName: String);
    procedure Reset;
    procedure LogCommand(pPar1: String; pPar2: String);
    procedure SendMessage(pMessage: TXplMessage);
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
  fCallbackCount := 0;
  fXplVariableValue := nil;
  fRequestSequence := 1;
  fCallbacks := TCallbackInfoMap.Create;
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
  for I := fCallbacks.Count - 1 downto 0 do
    fCallbacks.Data[I].Free;
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
    lCom^.Header.Status:=STATUS_READY;
    DebugLog('Sending LMC started command to reset XPL plugin structures');
  end;
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
    lCom^.Header.Status:=STATUS_READY;
    DebugLog(Format('Sending DrawText command for text %s at pos %f.', [pText, pPos]));
    //DebugLogFmt('Size of TLmc2XplSharedMem is %d', [SizeOf(TLmc2XplSharedMem)]);
  end;
end;

procedure TXPLcontrol.XplVarProcessed;
begin
  if fXplVariableValue <> nil then
  begin
    fXplVariableValue.Free;
    fXplVariableValue := nil;
  end else
    Glb.LogError('Value from XPL should be markes as processed, but there''s no such value', cLoggerXpl);
end;

procedure TXPLcontrol.SetVariableHook(pVarName: String; pHandlerRef: Integer;
  pIntervalMs: Integer; pDelta: Integer);
var
  lXplObj: TXplVariableCallback;
  lCbInfo: TLmcVariableCallbackInfo;
  lId: Int64;
begin
  lId:=Glb.UnixTimestampMs * 100 + fCallbackCount;
  Inc(fCallbackCount);
  lXplObj := TXplVariableCallback.Create(pVarName, pIntervalMs, pDelta, lId);
  //fXplSender.SendMessage(lXplObj);
  lCbInfo := TLmcVariableCallbackInfo.Create;
  lCbInfo.Id:=lId;
  lCbInfo.Interval:=pIntervalMs;
  lCbInfo.Delta:=pDelta;
  lCbInfo.Name:=pVarName;
  lCbInfo.LuaHandlerRef:=pHandlerRef;
  fCallbacks.Add(lId, lCbInfo);
  Glb.DebugLog(Format('Registered variable callback for %s with id %d, interval %d and delta %d',
    [pVarName, lId, pIntervalMs, pDelta]), cLoggerXpl);
  lXplObj.Free;
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
    if UpperCase(pVarName) = UpperCase(fCallbacks.Data[I].Name) then
    begin
      Glb.DebugLog(Format('Removing registered variable %s with id %d and interval %d',
        [fCallbacks.Data[I].Name, fCallbacks.Keys[i], fCallbacks.Data[I].Interval]), cLoggerXpl);
      fCallbacks.Data[I].Free;
      fCallbacks.Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure TXPLcontrol.Reset;
begin
  //TODO: Send unhook-all command to XPL
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

procedure TXPLcontrol.SendMessage(pMessage: TXplMessage);
begin
  //fXplSender.SendMessage(pMessage);
end;

procedure TXPLcontrol.OnXplSyncMessage(Sender: TObject);
var
  lStream: TMemoryStream;
  lMessageType: byte;
begin
  Glb.DebugLog('Sync message from XPL arrived.', cLoggerXpl);
  lStream := TMemoryStream.Create;
  try
    try
      //fXplSyncListener.Server.GetMessageData(lStream);
      Glb.DebugLog('Received message with length ' + IntToStr(lStream.Size), cLoggerXpl);
      lStream.Position:=0;
      lMessageType := lStream.ReadByte;
      if (lMessageType = HDMC_RECONNECT) then
      begin
        //fXplSender.Reconnect;
      end else if (lMessageType = HDMC_VAR_RESPONSE) then
      begin
        if (fXplVariableValue <> nil) then
        begin
          Glb.LogError(Format('Unprocessed variable value %s', [fXplVariableValue.Name]), cLoggerXpl);
        end
        else
        begin
          fXplVariableValue := TXplVariableValue.Create(lStream);
          Glb.DebugLog('Got variable response, fXplVariableValue set to ' + fXplVariableValue.ToString, cLoggerXpl);
        end;
      end else
        Glb.LogError(Format('Unexpected message from Xplane with type %d', [lMessageType]), cLoggerXpl);
    except
      on E:Exception do
        Glb.LogError(Format('Pipe exception: %s', [E.Message]), cLoggerXpl);
    end;
  finally
    lStream.Free;
  end;
end;

procedure TXPLcontrol.OnXplAsyncMessage(Sender: TObject);
var
  lStream: TMemoryStream;
  lMessageType: byte;
  lVarValue: TXplVariableValue;
  lCallbackInfo: TLmcVariableCallbackInfo;
begin
  Glb.DebugLog('Async message from XPL arrived.', cLoggerXpl);
  lStream := TMemoryStream.Create;
  try
    try
      //fXplAsyncListener.Server.GetMessageData(lStream);
      Glb.DebugLog('Received async message with length ' + IntToStr(lStream.Size), cLoggerXpl);
      lStream.Position:=0;
      lMessageType := lStream.ReadByte;
      if (lMessageType = HDMC_RECONNECT) then
      begin
        //fXplSender.Reconnect;
      end else if (lMessageType = HDMC_VAR_RESPONSE) then
      begin
        lVarValue := TXplVariableValue.Create(lStream);
        Glb.DebugLog('Got variable response with value ' + lVarValue.ToString, cLoggerXpl);
        try
          lCallbackInfo := fCallbacks.KeyData[lVarValue.Id];
        except
          on E:EListError do
            lCallbackInfo := nil;
        end;
        if (lCallbackInfo = nil) then
          Glb.LogError(Format('Callback for variable %s with id %d not found.', [lVarValue.Name, lVarValue.Id]), cLoggerXpl)
        else
        begin
          Glb.DebugLog(Format('Calling Lua function %d with change count %d.',
              [lCallbackInfo.LuaHandlerRef, lVarValue.ChangeCount]), cLoggerXpl);
          Glb.LuaEngine.CallFunctionByRef(lCallbackInfo.LuaHandlerRef, lVarValue, lVarValue.ChangeCount);
        end;
      end else
        Glb.LogError(Format('Unexpected message from Xplane with type %d', [lMessageType]), cLoggerXpl);
    except
      on E:Exception do
        Glb.LogError(Format('Pipe exception: %s', [E.Message]), cLoggerXpl);
    end;
  finally
    lStream.Free;
  end;
end;

function TXPLcontrol.getCommandSlotAndInitHeader: PLmcCommandRec;
var
  lIndex: Integer;
  lCommand: PLmcCommandRec;
begin
  lCommand:=nil;
  for lIndex := Low(fLmcMem^.Commands) to High(fLmcMem^.Commands) do
  begin
    if fLmcMem^.Commands[lIndex].Header.Status = STATUS_FREE then
    begin
      lCommand:=@fLmcMem^.Commands[lIndex];
      break;
    end;
    if (fLmcMem^.Commands[lIndex].Header.Status = STATUS_READY) and
      (fXplMem.LastProcessedId > fLmcMem^.Commands[lIndex].Header.Id) then
    begin
      lCommand:=@fLmcMem^.Commands[lIndex];
      lCommand.Header.Status:=STATUS_FREE;
      break;
    end;
  end;
  if (lCommand = nil) then
  begin
    DebugLog(Format('No free slots (out of %d) for XPL command. Xplane probably not running.', [High(fLmcMem^.Commands) - Low(fLmcMem^.Commands)]));
    Result := nil;
  end else begin
    Result := lCommand;
    Result^.Header.Status:=STATUS_WRITING;
    Result^.Header.Id:=fRequestSequence;
    Result^.Header.TimeStamp:=Glb.UnixTimestampMs;
    DebugLog(Format('Initiated command slot %d with request id %d at %d.', [lIndex, fRequestSequence, Result^.Header.TimeStamp]));
    Inc(fRequestSequence);
    DebugLog(Format('XPL memory: last id %d, updated %d.', [fXplMem^.LastProcessedId, fXplMem^.UpdateTimeStamp]));
  end;
end;

function TXPLcontrol.GetVariableValueFromXplSharedMemory(pName: String;
  pIndex: Integer): TXplValue;
begin

end;

function TXPLcontrol.GetXplVariable(pName: String; pIndex: Integer): TXplValue;
var
  lCom: PLmcCommandRec;
  lTickCount: ULONGLONG;
begin
  Result := GetVariableValueFromXplSharedMemory(pName, pIndex);
  if (Result <> nil) then
    exit;
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_GET_VARIABLE;
    lCom^.GetVariableData.Name:=pName;
    lCom^.GetVariableData.Index:=pIndex;
    lCom^.Header.Status:=STATUS_READY;
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
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_SET_VARIABLE;
    lCom^.SetVariableData.Name:=pName;
    lCom^.SetVariableData.Value := pValue.Value;
    lCom^.SetVariableData.Index:=pIndex;
    lCom^.Header.Status:=STATUS_READY;
    DebugLog(Format('Sending SetVar command for variable %s index %d.', [pName, pIndex]));
  end;
end;

procedure TXPLcontrol.IncVariable(pData: TXplIncVariableRec);
var
  lCom: PLmcCommandRec;
begin
  lCom := getCommandSlotAndInitHeader;
  if (lCom <> nil) then
  begin
    lCom^.CommandType:=LMC_COMMAND_INC_VARIABLE;
    lCom^.IncVariableData := pData;
    lCom^.Header.Status:=STATUS_READY;
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
    lCom^.Header.Status:=STATUS_READY;
    DebugLog(Format('Sending execute command %s.', [pCmdName]));
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
    lCom^.Header.Status:=STATUS_READY;
    DebugLog(Format('Sending begin execute command %s.', [pCmdName]));
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
    lCom^.Header.Status:=STATUS_READY;
    DebugLog(Format('Sending end execute command %s.', [pCmdName]));
  end;
end;


end.
