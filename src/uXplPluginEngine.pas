unit uXplPluginEngine;

{$Define DEBUG}

interface

uses XPLMDataAccess, XPLMUtilities, uXplCommon, uXplSender, uXplPluginReceiver, uXplMessages, classes, fgl, uTiming;

type

TCallbackInfoMap = TFPGMap<Int64,TXplVariableCallbackInfo>;

{ TXplEngine }

TXplEngine = class (TObject)
    fDebugging: Boolean;
    fTickProfilingLog: String;
    fMessageProfilingLog: String;
    fTickTimer: TStopWatch;
    fMessageTimer: TStopWatch;
    fDebugLogFileName: String;
    fTextToBeDrawn: String;
    fScreenWidth: Integer;
    fScreenHeight: Integer;
    fBasicFontHeight: Integer;
    fBasicFontWidth: Integer;
    fTextFloatPosition: Single;
    fTextHideTs: TDateTime;
    fSyncSender: TXplSender;
    fAsyncSender: TXplSender;
    fReceiver: TXplPluginReceiver;
    fDataRefs: TStringList;
    fCommands: TStringList;
    fVarCallbacks: TCallbackInfoMap;
    procedure DebugLog(Value: String);
    procedure DebugLogFmt(pFormat:String; pArgs: array of const);
    function GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
    procedure String2XplValue(pIn:String; pOut: PXplValue; pDataType: XPLMDataTypeID);
    procedure InitGlValues;
    procedure OnLmcMessage(pSender: TObject);
    procedure XplDebugFmt(pFormat:String; pArgs: array of const);
    procedure RunAndFree(pText: TXplDrawText); overload;
    procedure RunAndFree(pVar: TXplSetVariable); overload;
    procedure RunAndFree(pVar: TXplGetVariable); overload;
    procedure RunAndFreeCommand(pComm: TXplCallWithName);
    procedure RunAndFree(pVar: TXplVariableCallback); overload;
    procedure RunAndFree(pVar: TXplUnhookVariable); overload;
    procedure RunAndFree(pVar: TXplSetLogFile); overload;
    function GetOrRegisterXplVariable(pName: String): TXplVariable;
    function GetOrRegisterXplCommand(pName: String): XPLMCommandRef;
    function GetOrRegisterXplVariableCallback(pId: Int64; pName: String): TXplVariableCallbackInfo;
    procedure SetVariable(pDef: TXplVariable; pVal: TXplValue; pIndex: Int64);
    function GetVariable(pDef: TXplVariable; pProduceLog: Boolean): TXplValue; overload;
    function GetVariable(pDef: TXplVariable; pIndex: Integer; pProduceLog: Boolean): TXplValue; overload;
    procedure ReconnectSenders;
    procedure CheckVariableCallbacks;
    function UnixTimestampMs: Int64;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure XplTick;
    procedure DrawText();
end;

implementation

uses SysUtils, XPLMGraphics, gl, glu, XPLMDisplay, dateutils;

{ TXplVariableCallbackInfo }

{ TXplEngine }

const
  cLogFileTriggerName = 'lmc_log_file_trigger.log';

constructor TXplEngine.Create;
begin
  if (FileExists(cLogFileTriggerName)) then
  begin
    fDebugging:=true;
    fDebugLogFileName:=cLogFileTriggerName;
    DebugLog('LuaMacros Plugin started');
  end
  else
    fDebugging := false;
  fTickTimer := TStopWatch.Create;
  fMessageTimer := TStopWatch.Create;
  fTextToBeDrawn:='';
  fTextFloatPosition := 0;
  fSyncSender := TXplSender.Create(cXplToLmcPipeName);
  fSyncSender.DebugMethod:=DebugLogFmt;
  fAsyncSender := TXplSender.Create(cXplToLmcAsyncPipeName);
  fAsyncSender.DebugMethod:=DebugLogFmt;
  fReceiver := TXplPluginReceiver.Create(cLmcToXplPipeName);
  fReceiver.OnMessage:=OnLmcMessage;
  fReceiver.Init;
  fDataRefs:=TStringList.Create;
  fDataRefs.CaseSensitive:=False;
  fCommands:=TStringList.Create;
  fCommands.CaseSensitive:=False;
  fVarCallbacks:=TCallbackInfoMap.Create;
  if (fSyncSender.ServerRunning) then
  begin
    // LMC is already running, but its sender could be connected to nowhere (old XPL instance)
    // send it reconnect request
    DebugLog('LMC connected, sending message');
    fSyncSender.SendMessage(TXplReconnectToServer.Create);
  end else
    DebugLog('LMC not connected');
  InitGlValues;
  DebugLog('LuaMacros init done');
end;

procedure TXplEngine.DebugLog(Value: String);
var
  lVal: String;
  logFile: TextFile;
begin
  if not fDebugging then
    exit;
  lVal := Format('%s [XPLLUMplugin]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), Value]);
  // to file
  AssignFile(logFile, fDebugLogFileName);
  if FileExists(fDebugLogFileName) then
      Append(logFile)
    else
      Rewrite(logFile);
  WriteLn(logFile, lVal);
  CloseFile(logFile);
end;

procedure TXplEngine.DebugLogFmt(pFormat: String; pArgs: array of const);
begin
  DebugLog(Format(pFormat, pArgs));
end;

destructor TXplEngine.Destroy;
var
  I: Integer;
begin
  fReceiver.Free;
  fSyncSender.Free;
  fAsyncSender.Free;
  fTickTimer.Free;
  fMessageTimer.Free;
  for I := 0 to fDataRefs.Count - 1 do
  begin
    fDataRefs.Objects[I].Free;
  end;
  for I := 0 to fVarCallbacks.Count - 1 do
  begin
    fVarCallbacks.Data[I].Free;
  end;
  fDataRefs.Free;
  fCommands.Free;
  fVarCallbacks.Free;
  inherited;
end;

function TXplEngine.GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
begin
  Result := 0;
  if (pDataType = xplmType_IntArray) then
    Result := XPLMGetDatavi(pDataRef, nil, 0, 0);
  if (pDataType = xplmType_FloatArray) then
    Result := XPLMGetDatavf(pDataRef, nil, 0, 0);
  if (pDataType = xplmType_Data) then
    Result := XPLMGetDatab(pDataRef, nil, 0, 0);
end;

procedure TXplEngine.XplTick;
begin
  if (fDebugging) then
  begin
    fTickTimer.Start;
  end;
  CheckVariableCallbacks;
  if (fDebugging) then
  begin
    fTickProfilingLog:=fTickProfilingLog + #10 + #13 + fTickTimer.StopAndLog;
    if (Length(fTickProfilingLog) > 2000) then
    begin
      DebugLog('Tick log: ' + fTickProfilingLog);
      fTickProfilingLog:='';
    end;
  end;

end;

procedure TXplEngine.String2XplValue(pIn: String; pOut: PXplValue; pDataType: XPLMDataTypeID);
var
  lBuff: PChar;
  lMessage: String;
  FS: TFormatSettings;
begin
  FillChar(FS, SizeOf(FS), 0);
  //FS.ThousandSeparator := ',';
  FS.DecimalSeparator := '.';
  DebugLog(Format('Converting value %s.', [pIn]));
  // convert from string to appropriate value
  try
    case pDataType of
    xplmType_Float,
    xplmType_FloatArray:
      pOut^.floatData := StrToFloat(pIn, FS);
    xplmType_Double:
      pOut^.doubleData := StrToFloat(pIn, FS);
    xplmType_Int,
        xplmType_IntArray:
      pOut^.intData := StrToint(pIn);
    end;
  except
    on E: EConvertError do
    begin
      lMessage:=Format('Converting error in value %s. %s', [pIn, e.Message]);
      DebugLog(lMessage);
      GetMem(lBuff, Length(lMessage) + 1);
      try
        StrPCopy(lBuff, lMessage);
        XPLMDebugString(lBuff);
      finally
        FreeMem(lBuff);
      end;
      pOut^.floatData := 0;
      pOut^.doubleData := 0;
      pOut^.intData := 0;
    end;
  end;
end;

procedure TXplEngine.InitGlValues;
begin
  XPLMGetScreenSize(@fScreenWidth, @fScreenHeight);
  XPLMGetFontDimensions(xplmFont_Basic, @fBasicFontWidth, @fBasicFontHeight, nil);
end;

procedure TXplEngine.OnLmcMessage(pSender: TObject);
var
  lStream: TMemoryStream;
  lMessageType: byte;
begin
  if (fDebugging) then
  begin
    fMessageTimer.Start;
  end;

  lStream := TMemoryStream.Create;
  try
    try
      fReceiver.Server.GetMessageData(lStream);
      lStream.Position:=0;
      lMessageType := lStream.ReadByte;
      case lMessageType of
        HDMC_SHOW_TEXT: RunAndFree(TXplDrawText.Create(lStream));
        HDMC_SET_VAR: RunAndFree(TXplSetVariable.Create(lStream));
        HDMC_GET_VAR: RunAndFree(TXplGetVariable.Create(lStream));
        HDMC_EXEC_COMMAND: RunAndFreeCommand(TXplExecuteCommand.Create(lStream));
        HDMC_COMMAND_BEGIN: RunAndFreeCommand(TXplExecuteCommandBegin.Create(lStream));
        HDMC_COMMAND_END: RunAndFreeCommand(TXplExecuteCommandEnd.Create(lStream));
        HDMC_RECONNECT: ReconnectSenders;
        HDMC_VAR_CALLBACK: RunAndFree(TXplVariableCallback.Create(lStream));
        HDMC_UNHOOK_VAR: RunAndFree(TXplUnhookVariable.Create(lStream));
        HDMC_SET_LOG_FILE: RunAndFree(TXplSetLogFile.Create(lStream));
      end;
    except
      on E:Exception do
        XplDebugFmt('Pipe exception: %s', [E.Message]);
    end;
  finally
    lStream.Free;
  end;

  if (fDebugging) then
  begin
    fMessageProfilingLog:=fMessageProfilingLog + #10 + #13 + fMessageTimer.StopAndLog;
    if (Length(fMessageProfilingLog) > 1000) then
    begin
      DebugLog('On mesage log: ' + fMessageProfilingLog);
      fMessageProfilingLog:='';
    end;
  end;

end;

procedure TXplEngine.XplDebugFmt(pFormat: String; pArgs: array of const);
var
 lBuff: PChar;
 lMessage: String;
begin
  lMessage:=Format(pFormat, pArgs) + #13;
  GetMem(lBuff, Length(lMessage) + 1);
  try
    StrPCopy(lBuff, lMessage);
    XPLMDebugString(lBuff);
  finally
    FreeMem(lBuff);
  end;
end;

procedure TXplEngine.RunAndFree(pText: TXplDrawText);
begin
  fTextFloatPosition := pText.Position;
  fTextToBeDrawn := pText.Text;
  if (pText.TimeInSec > 0) then
    fTextHideTs := IncSecond(Now(), pText.TimeInSec)
  else
    fTextHideTs := 0;
  DebugLog(Format('Received DrawText %s at pos %f.', [fTextToBeDrawn, fTextFloatPosition]));
  pText.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplSetVariable);
var
  lXV: TXplVariable;
begin
  if (pVar.Index = NO_INDEX) then
    DebugLog('Received request to set variable ' + pVar.Name + ' to ' + pVar.Value.ToString)
  else
    DebugLogFmt('Received request to set variable %s[%d] to %s', [pVar.Name, pVar.Index, pVar.Value.ToString]);
  lXV := GetOrRegisterXplVariable(pVar.Name);
  if (lXV <> nil) then
  begin
    if (lXV.Writable) then
    begin
      DebugLog('Variable ' + pVar.Name + ' is writable, setting...');
      SetVariable(lXV, pVar.Value, pVar.Index)
    end
    else
      DebugLog('Variable ' + pVar.Name + ' is not writable.');
  end
  else
    DebugLog('Cannot set variable ' + pVar.Name + ' - not found.');
  pVar.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplGetVariable);
var
  lXV: TXplVariable;
  lValue: TXplValue;
begin
  if (pVar.Index = NO_INDEX) then
  begin
    DebugLog(Format('Received request id [%d] to get variable %s.', [pVar.Id, pVar.Name]));
  end
  else
  begin
    DebugLog(Format('Received request id %d to get variable %s[%d].', [pVar.Id, pVar.Name, pVar.Index]));
  end;
  lXV := GetOrRegisterXplVariable(pVar.Name);
  if (lXV <> nil) then
  begin
    lValue := GetVariable(lXV, pVar.Index, True);
    if (lValue <> nil) then
    begin
      fSyncSender.SendMessage(TXplVariableValue.Create(pVar.Name, lValue, pVar.Id));
      DebugLog('Written to stream');
    end
    else
      DebugLog('Can not find out value of variable ' + pVar.Name);
  end
  else
    DebugLog('Variable ' + pVar.Name + ' not found.');
  pVar.Free;
end;

procedure TXplEngine.RunAndFreeCommand(pComm: TXplCallWithName);
var
  lCom: XPLMCommandRef;
begin
  lCom:=GetOrRegisterXplCommand(pComm.Name);
  if (lCom <> nil) then
  begin
    if (pComm is TXplExecuteCommandBegin) then
    begin
      XPLMCommandBegin(lCom);
      DebugLog(Format('Executed command begin %s.', [pComm.Name]));
    end else
    if (pComm is TXplExecuteCommandEnd) then
    begin
      XPLMCommandEnd(lCom);
      DebugLog(Format('Executed command end %s.', [pComm.Name]));
    end else
    begin
      XPLMCommandOnce(lCom);
      DebugLog(Format('Executed command %s.', [pComm.Name]));
    end;
  end;
  pComm.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplVariableCallback);
var
  lXVC: TXplVariableCallbackInfo;
begin
  DebugLog(Format('Received request for variable %s callback with id %d.', [pVar.Name, pVar.Id]));
  lXVC := GetOrRegisterXplVariableCallback(pVar.Id, pVar.Name);
  if (lXVC <> nil) then
  begin
    lXVC.Interval:=pVar.IntervalMs;
    lXVC.Delta:=pVar.Delta;
    lXVC.Id:=pVar.Id;
    DebugLog(Format('Variable %s registered with id %d, interval %d ms and delta %d.', [pVar.Name, pVar.Id, pVar.IntervalMs, pVar.Delta]));
  end
  else
    DebugLog('Variable ' + pVar.Name + ' not found.');
  pVar.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplUnhookVariable);
var
  I:Integer;
begin
  // remove from my records
  i := 0;
  while i < fVarCallbacks.Count do
  begin
    if UpperCase(pVar.Name) = UpperCase(fVarCallbacks.Data[I].XplVariable.Name) then
    begin
      DebugLogFmt('Removing registered variable %s with id %d and interval %d',
        [fVarCallbacks.Data[I].XplVariable.Name, fVarCallbacks.Keys[i], fVarCallbacks.Data[I].Interval]);
      fVarCallbacks.Data[I].Free;
      fVarCallbacks.Delete(i);
    end
    else
      Inc(i);
  end;
  pVar.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplSetLogFile);
begin
  fDebugging:=True;
  fDebugLogFileName:=pVar.Name;
  DebugLogFmt('Received log file name %s', [fDebugLogFileName]);
  pVar.Free;
end;

function TXplEngine.GetOrRegisterXplVariable(pName: String): TXplVariable;
var
  lIndex: Integer;
  lXV: TXplVariable;
  lDR: XPLMDataRef;
begin
  Result := nil;
  lIndex := fDataRefs.IndexOf(pName);
  if (lIndex >= 0) then
  begin
    Result := TXplVariable(fDataRefs.Objects[lIndex]);
    DebugLog('Variable ' + pName + ' already known with offset ' + IntToStr(Int64(Result.DataRef)));
  end
  else
  begin
    lDr := XPLMFindDataRef(PChar(pName));
    if (lDr <> nil) then
    begin
      lXV := TXplVariable.Create;
      LXV.Name:=pName;
      lXV.DataRef:=lDR;
      lXV.Writable:=(XPLMCanWriteDataRef(lDR) <> 0);
      lXV.DataType:=XPLMGetDataRefTypes(lDR);
      lXV.Length:=GetArrayLength(lDR, lXV.DataType);
      fDataRefs.AddObject(pName, lXV);
      Result := lXV;
      DebugLog('Variable ' + pName + ' located with offset ' + IntToStr(Int64(Result.DataRef)));
    end
    else
      DebugLog('XPL doesn''t know variable ' + pName);
  end;
end;

function TXplEngine.GetOrRegisterXplCommand(pName: String): XPLMCommandRef;
var
  lIndex: Integer;
  lXV: TXplVariable;
  lDR: XPLMDataRef;
begin
  Result := nil;
  lIndex := fCommands.IndexOf(pName);
  if (lIndex >= 0) then
  begin
    Result := XPLMCommandRef(fCommands.Objects[lIndex]);
    DebugLog('Command ' + pName + ' already known with offset ' + IntToStr(Int64(Result)));
  end
  else
  begin
    Result := XPLMFindCommand(PChar(pName));
    if (Result <> nil) then
    begin
      fCommands.AddObject(pName, Result);
      DebugLog('Command ' + pName + ' located with offset ' + IntToStr(Int64(Result)));
    end
    else
      DebugLog('XPL doesn''t know command ' + pName);
  end;
end;

function TXplEngine.GetOrRegisterXplVariableCallback(pId: Int64; pName: String): TXplVariableCallbackInfo;
var
  lIndex: Integer;
  lXV: TXplVariable;
  lXVC: TXplVariableCallbackInfo;
begin
  Result := nil;
  lIndex := fVarCallbacks.IndexOf(pId);
  if (lIndex >= 0) then
  begin
    Result := fVarCallbacks.Data[lIndex];
    DebugLog('Variable ' + Result.XplVariable.Name + ' already registered with interval ' + IntToStr(Int64(Result.Interval)));
  end
  else
  begin
    lXV := GetOrRegisterXplVariable(pName);
    if (lXV <> nil) then
    begin
      lXVC := TXplVariableCallbackInfo.Create;
      lXVC.XplVariable := lXV;
      fVarCallbacks.Add(pId, lXVC);
      Result := lXVC;
    end else begin
      DebugLog('Can not register variable callback because XPL doesn''t know variable ' + pName);
    end;
  end;
end;

procedure TXplEngine.SetVariable(pDef: TXplVariable; pVal: TXplValue; pIndex: Int64);
var
  lSingle: Single;
  lInteger: Integer;
begin
  DebugLogFmt('About to set type %d of variable %s', [Ord(pDef.DataType), pDef.Name]);
  case pDef.DataType of
    xplmType_Float:
    begin
      pVal.MakeDouble;
      DebugLog('Setting variable ' + pDef.Name + ' to float value ' + FloatToStr(pVal.DoubleValue));
      XPLMSetDataf(pDef.DataRef, pVal.DoubleValue);
    end;
    xplmType_Double:
    begin
      pVal.MakeDouble;
      DebugLog('Setting variable ' + pDef.Name + ' to double value ' + FloatToStr(pVal.DoubleValue));
      XPLMSetDatad(pDef.DataRef, pVal.DoubleValue);
    end;
    xplmType_Int:
    begin
      pVal.MakeInt;
      DebugLog('Setting variable ' + pDef.Name + ' to int value ' + IntToStr(pVal.IntValue));
      XPLMSetDatai(pDef.DataRef, pVal.IntValue);
    end;
    xplmType_Data:
    begin
      pVal.MakeString;
      //XPLMSetDatab(Pointer8b2Pointer(pDef), pVal.IntValue);
      DebugLog('Setting string not yet implemented');
    end;
    xplmType_IntArray:
    begin
      pVal.MakeInt;
      DebugLogFmt('Setting variable %s[%d] to int value %d', [pDef.Name, pIndex, pVal.IntValue]);
      lInteger:=pVal.IntValue;
      XPLMSetDatavi(pDef.DataRef, @lInteger, pIndex, 1);
    end;
    xplmType_FloatArray:
    begin
      pVal.MakeDouble;
      DebugLogFmt('Setting variable %s[%d] to double value %f', [pDef.Name, pIndex, pVal.DoubleValue]);
      lSingle := pVal.DoubleValue;
      XPLMSetDatavf(pDef.DataRef, @lSingle, pIndex, 1);
    end;
    else
    begin
      DebugLogFmt('Unknown type %d of variable %s', [Ord(pDef.DataType), pDef.Name]);
    end;
  end;
  DebugLogFmt('Setting done %d of variable %s', [Ord(pDef.DataType), pDef.Name]);
end;

function TXplEngine.GetVariable(pDef: TXplVariable; pProduceLog: Boolean): TXplValue;
begin
  Result := GetVariable(pDef, NO_INDEX, pProduceLog);
end;

function TXplEngine.GetVariable(pDef: TXplVariable; pIndex: Integer; pProduceLog: Boolean): TXplValue;
var
  lBuff: array[0..500] of char;
  lBuffPtr: PChar;
  lLength: Integer;
  lSingle: Single;
  lReal: Real;
  lInt: Integer;
begin

  case Ord(pDef.DataType) of
    Ord(xplmType_Float):
    begin
      lSingle:=XPLMGetDataf(pDef.DataRef);
      if pProduceLog then
        DebugLog(Format('Got float value %f of variable %s.', [lSingle, pDef.Name]));
      Result := TXplValue.Create(lSingle);
    end;
    Ord(xplmType_Double), Ord(xplmType_Double) + Ord(xplmType_Float):
    begin
      lReal:=XPLMGetDatad(pDef.DataRef);
      if pProduceLog then
        DebugLog(Format('Got double value %f of variable %s.', [lReal, pDef.Name]));
      Result := TXplValue.Create(lReal);
    end;
    Ord(xplmType_Int):
    begin
      lInt:=XPLMGetDatai(pDef.DataRef);
      if pProduceLog then
        DebugLog(Format('Got int value %d of variable %s.', [lInt, pDef.Name]));
      Result := TXplValue.Create(lInt);
    end;
    Ord(xplmType_Data):
    begin
      if (pDef.Length > 500) then
        lLength:=500
      else
        lLength:=pDef.Length;
      lBuffPtr:=lBuff;
      XPLMGetDatab(pDef.DataRef, lBuffPtr, 0, lLength);
      if pProduceLog then
        DebugLog('Got string value of variable ' + pDef.Name + ': ' + lBuff);
      Result := TXplValue.Create(lBuff);
    end;
    Ord(xplmType_FloatArray):
    begin
      XPLMGetDatavf(pDef.DataRef, @lSingle, pIndex, 1);
      if pProduceLog then
        DebugLog(Format('Got float value %f of variable %s.', [lSingle, pDef.Name]));
      Result := TXplValue.Create(lSingle);
    end;
    Ord(xplmType_IntArray):
    begin
      XPLMGetDatavi(pDef.DataRef, @lInt, pIndex, 1);
      if pProduceLog then
        DebugLog(Format('Got int value %d of variable %s.', [lInt, pDef.Name]));
      Result := TXplValue.Create(lInt);
    end;
    else
    begin
      DebugLog(Format('Unknown type %d of variable %s.', [pDef.DataType, pDef.Name]));
    end;
  end;
end;

procedure TXplEngine.ReconnectSenders;
begin
  fSyncSender.Reconnect;
  fAsyncSender.Reconnect;
end;

procedure TXplEngine.CheckVariableCallbacks;
var
  I: Integer;
  lXVC: TXplVariableCallbackInfo;
  lValue: TXplValue;
  lChanged: boolean;
  lSend: boolean;
  lNow: Int64;
begin
  for I := 0 to Pred(fVarCallbacks.Count) do
  begin
    lXVC := TXplVariableCallbackInfo(fVarCallbacks.Data[I]);
    // get value
    lValue := GetVariable(lXVC.XplVariable, False);
    if (lValue = nil) then
      Continue; // can not find out variable value
    // compare with last from info

    lChanged:= (lXVC.LastValue = nil); // first assignment
    if (not lChanged) then
    begin
      if (lXVC.Delta = 0) then
        lChanged := not lValue.Equals(lXVC.LastValue) // different value
      else
        lChanged := not lValue.EqualsWithDelta(lXVC.LastValue, lXVC.Delta) // different value
    end;
    if (lChanged) or (lXVC.ChangeCount > 0) then
    begin
      if lChanged then
      begin
        // set current value as last, so first free last value
        if (lXVC.LastValue <> nil) then
          lXVC.LastValue.Free;
        lXVC.LastValue := lValue;
        Inc(lXVC.ChangeCount);
      end;
      // should we send the change? Calculate interval
      lNow := UnixTimestampMs;
      lSend := (lNow - lXVC.LastCallback) >= lXVC.Interval;
      if (lSend) then
      begin
        fAsyncSender.SendMessage(TXplVariableValue.Create(lXVC.XplVariable.Name, lValue, lXVC.Id, lXVC.ChangeCount));
        DebugLog(Format('Change of variable %s written to stream with value %s and change count %d',
          [lXVC.XplVariable.Name, lValue.ToString, lXVC.ChangeCount]));
        lXVC.ChangeCount:=0;
        lXVC.LastCallback:=lNow;
      end;
    end
    else
      lValue.Free;
  end;
end;

function TXplEngine.UnixTimestampMs: Int64;
begin
  Result := Round(Now * 24*60*60*1000);
end;

procedure TXplEngine.DrawText;
var
  rgb : array[0..2] of single;
  lTextYPos : Integer;
begin
  if (fTextToBeDrawn = '') then
    exit;
  if (fTextHideTs > 0) and (fTextHideTs - Now() < 0) then
  begin
    fTextToBeDrawn := '';
    fTextHideTs := 0;
    exit;
  end;
  rgb[0] := 1;
  rgb[1] := 1;
  rgb[2] := 1;
  //XPLMSetGraphicsState(0, 0, 0, 0, 0, 0, 0); // turn off blending
  XPLMGetScreenSize(@fScreenWidth, @fScreenHeight);
  lTextYPos := Round(fScreenHeight*(1-fTextFloatPosition))-fBasicFontHeight;
  XPLMDrawTranslucentDarkBox(8, lTextYPos + fBasicFontHeight + 3, 12 + fBasicFontWidth * Length(fTextToBeDrawn), lTextYPos - 6);
  XPLMDrawString(@rgb, 10, lTextYPos, PAnsiChar(fTextToBeDrawn), nil, xplmFont_Basic);
end;

end.
