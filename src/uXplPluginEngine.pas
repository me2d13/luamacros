unit uXplPluginEngine;

{$Define DEBUG}

interface

uses XPLMDataAccess, XPLMUtilities, uXplCommon, uXplSender, uXplPluginReceiver, uXplMessages, classes, fgl;

type

TCallbackInfoMap = TFPGMap<Int64,TXplVariableCallbackInfo>;

{ TXplEngine }

TXplEngine = class (TObject)
    fDebugging: Boolean;
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
    procedure RunAndFree(pComm: TXplExecuteCommand); overload;
    procedure RunAndFree(pVar: TXplVariableCallback); overload;
    function GetOrRegisterXplVariable(pName: String): TXplVariable;
    function GetOrRegisterXplCommand(pName: String): XPLMCommandRef;
    function GetOrRegisterXplVariableCallback(pId: Int64; pName: String): TXplVariableCallbackInfo;
    procedure SetVariable(pDef: TXplVariable; pVal: TXplValue);
    function GetVariable(pDef: TXplVariable): TXplValue;
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

constructor TXplEngine.Create;
begin
  fDebugging := FileExists('luamacros.log');
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
    fSyncSender.SendMessage(TXplReconnectToServer.Create);
  end;
  InitGlValues;
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
  AssignFile(logFile, 'luamacros.log');
  if FileExists('luamacros.log') then
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
  CheckVariableCallbacks;
end;

{$IFDEF OLDWAY}
procedure TXplEngine.ProcessSlot(pSlot: PXplComSlot);
var
  lInt: Integer;
  lFloat: Single;
  lBuff: PChar;
  lMakeChecks: Boolean;
  lString: string;
begin
  if pSlot^.XplRequestFlag = 1 then
  begin
    // dump
    DebugLog('Slot dump, size of slot is ' + IntToStr(SizeOf(pSlot^)));
    //DebugLog(MemoryDump(pSlot, 1570));
    // check command
    if (pSlot^.HDMcommand = HDMC_GET_VAR) or
       (pSlot^.HDMcommand = HDMC_SET_VAR) or
       (pSlot^.HDMcommand = HDMC_TOGGLE_NEXT) or
       (pSlot^.HDMcommand = HDMC_TOGGLE_PREVIOUS) or
       (pSlot^.HDMcommand = HDMC_SWITCH_NEXT) or
       (pSlot^.HDMcommand = HDMC_SWITCH_PREVIOUS)
       then
    begin
      // is already registered?
      if pSlot^.DataRef = 0 then
      begin
        // register first
        DebugLog('Finding ref for ' + pSlot^.ValueName);
        pSlot^.DataRef := Pointer2Pointer8b(XPLMFindDataRef(pSlot^.ValueName));
        if pSlot^.DataRef <> 0 then
        begin
          DebugLog(Format('Got valid pointer %x.', [pSlot^.DataRef]));
          pSlot^.Writable := (XPLMCanWriteDataRef(Pointer8b2Pointer(pSlot^.DataRef)) <> 0);
          if (pSlot^.Writable) then
            DebugLog('Variable is writable.')
          else
            DebugLog('Variable is not writable.');
          pSlot^.DataType := XPLMGetDataRefTypes(Pointer8b2Pointer(pSlot^.DataRef));
          DebugLog('Data type is ' + IntToStr(Ord(pSlot^.DataType)));
          pSlot^.Length := GetArrayLength(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.DataType);
        end
        else
          DebugLog('Ref not found.');
        lMakeChecks := True; // we can make check only when all pBuffer items
            // were just filled
      end
      else
        lMakeChecks := False;
      if pSlot^.DataRef <> 0 then
      begin
        DebugLog(Format('Using data ref %x.', [pSlot^.DataRef]));
        if (pSlot^.HDMcommand = HDMC_SET_VAR) then
        begin
          String2XplValue(pSlot^.ValueUntyped, @(pSlot^.Value), pSlot^.DataType);
        end;
        if lMakeChecks and (not pSlot^.Writable) and (
            (pSlot^.HDMcommand = HDMC_SET_VAR) or
            (pSlot^.HDMcommand = HDMC_TOGGLE_NEXT) or
            (pSlot^.HDMcommand = HDMC_TOGGLE_PREVIOUS) or
            (pSlot^.HDMcommand = HDMC_SWITCH_NEXT) or
            (pSlot^.HDMcommand = HDMC_SWITCH_PREVIOUS)
            ) then
        begin
          DebugLog('Can''t set variable which is read only, chenging to get.');
          pSlot^.HDMcommand := HDMC_GET_VAR;
        end;
        if (pSlot^.HDMcommand = HDMC_GET_VAR) or
           (pSlot^.HDMcommand = HDMC_SET_VAR) then
          //SimpleReadWrite(pSlot, lMakeChecks);
        if (pSlot^.HDMcommand = HDMC_TOGGLE_NEXT) or
           (pSlot^.HDMcommand = HDMC_TOGGLE_PREVIOUS) or
           (pSlot^.HDMcommand = HDMC_SWITCH_NEXT) or
           (pSlot^.HDMcommand = HDMC_SWITCH_PREVIOUS) then
          //ToggleVar(pSlot);
          end;
    end;
    if (pSlot^.HDMcommand = HDMC_EXEC_COMMAND) or
       (pSlot^.HDMcommand = HDMC_COMMAND_BEGIN) or
       (pSlot^.HDMcommand = HDMC_COMMAND_END)
    then
    begin
      // is already registered?
      if pSlot^.CommandRef = 0 then
      begin
        // register first
        DebugLog('Finding ref for command ' + pSlot^.ValueName);
        pSlot^.CommandRef := Pointer2Pointer8b(XPLMFindCommand(pSlot^.ValueName));
        if pSlot^.CommandRef <> 0 then
        begin
          DebugLog(Format('Got valid pointer %p.', [Pointer8b2Pointer(pSlot^.CommandRef)]));
          end;
      end;
      if pSlot^.CommandRef <> 0 then
      begin
        DebugLog(Format('Sending command for %p.', [Pointer8b2Pointer(pSlot^.CommandRef)]));
        case pSlot^.HDMcommand of
          HDMC_EXEC_COMMAND: XPLMCommandOnce(Pointer8b2Pointer(pSlot^.CommandRef));
          HDMC_COMMAND_BEGIN: XPLMCommandBegin(Pointer8b2Pointer(pSlot^.CommandRef));
          HDMC_COMMAND_END: XPLMCommandEnd(Pointer8b2Pointer(pSlot^.CommandRef));
        end;
      end;
    end;
    if (pSlot^.HDMcommand = HDMC_SET_POSINTERVAL) then
      //fPosCountDown := pBuffer^.PosInterval;
    if (pSlot^.HDMcommand = HDMC_SHOW_TEXT) then
    begin
      fTextFloatPosition := pSlot^.Value.floatData;
      fTextToBeDrawn := pSlot^.StringBuffer;
      if (pSlot^.Length > 0) then
        fTextHideTs := IncSecond(Now(), pSlot^.Length)
      else
        fTextHideTs := 0;
      DebugLog(Format('Received DrawText %s at pos %f.', [fTextToBeDrawn, fTextFloatPosition]));
    end;
    pSlot^.XplRequestFlag := 0;
  end;
end;
{$EndIf}

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
        HDMC_EXEC_COMMAND: RunAndFree(TXplExecuteCommand.Create(lStream));
        HDMC_COMMAND_BEGIN: RunAndFree(TXplExecuteCommandBegin.Create(lStream));
        HDMC_COMMAND_END: RunAndFree(TXplExecuteCommandEnd.Create(lStream));
        HDMC_RECONNECT: ReconnectSenders;
        HDMC_VAR_CALLBACK: RunAndFree(TXplVariableCallback.Create(lStream));
      end;
    except
      on E:Exception do
        XplDebugFmt('Pipe exception: %s', [E.Message]);
    end;
  finally
    lStream.Free;
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
  DebugLog('Received request to set variable ' + pVar.Name + ' to ' + pVar.Value.ToString);
  lXV := GetOrRegisterXplVariable(pVar.Name);
  if (lXV <> nil) then
  begin
    if (lXV.Writable) then
      SetVariable(lXV, pVar.Value)
    else
      DebugLog('Variable ' + pVar.Name + ' is not writable.');
  end;
  pVar.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplGetVariable);
var
  lXV: TXplVariable;
  lValue: TXplValue;
begin
  DebugLog(Format('Received request id %d to get variable %s.', [pVar.Id, pVar.Name]));
  lXV := GetOrRegisterXplVariable(pVar.Name);
  if (lXV <> nil) then
  begin
    lValue := GetVariable(lXV);
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

procedure TXplEngine.RunAndFree(pComm: TXplExecuteCommand);
var
  lCom: XPLMCommandRef;
begin
  lCom:=GetOrRegisterXplCommand(pComm.Name);
  if (lCom <> nil) then
  begin
    if (pComm is TXplExecuteCommandBegin) then
    begin
      XPLMCommandBegin(lCom);
      DebugLog(Format('Executed command begin %s.', [pComm]));
    end else
    if (pComm is TXplExecuteCommandEnd) then
    begin
      XPLMCommandEnd(lCom);
      DebugLog(Format('Executed command end %s.', [pComm]));
    end else
    begin
      XPLMCommandOnce(lCom);
      DebugLog(Format('Executed command %s.', [pComm]));
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
    lXVC.Id:=pVar.Id;
    DebugLog(Format('Variable %s registered with id %d and interval %d ms.', [pVar.Name, pVar.Id, pVar.IntervalMs]));
  end
  else
    DebugLog('Variable ' + pVar.Name + ' not found.');
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

procedure TXplEngine.SetVariable(pDef: TXplVariable; pVal: TXplValue);
var
  lBuff: PChar;
begin
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
  end;
end;

function TXplEngine.GetVariable(pDef: TXplVariable): TXplValue;
var
  lBuff: array[0..500] of char;
  lBuffPtr: PChar;
  lLength: Integer;
  lSingle: Single;
  lReal: Real;
  lInt: Integer;
begin
  case pDef.DataType of
  xplmType_Float:
  begin
    lSingle:=XPLMGetDataf(pDef.DataRef);
    //DebugLog(Format('Got float value %f of variable %s.', [lSingle, pDef.Name]));
    Result := TXplValue.Create(lSingle);
  end;
  xplmType_Double:
  begin
    lReal:=XPLMGetDatad(pDef.DataRef);
    //DebugLog(Format('Got double value %f of variable %s.', [lReal, pDef.Name]));
    Result := TXplValue.Create(lReal);
  end;
  xplmType_Int:
  begin
    lInt:=XPLMGetDatai(pDef.DataRef);
    //DebugLog(Format('Got int value %d of variable %s.', [lInt, pDef.Name]));
    Result := TXplValue.Create(lInt);
  end;
  xplmType_Data:
  begin
    if (pDef.Length > 500) then
      lLength:=500
    else
      lLength:=pDef.Length;
    lBuffPtr:=lBuff;
    XPLMGetDatab(pDef.DataRef, lBuffPtr, 0, lLength);
    //DebugLog('Got string value of variable ' + pDef.Name + ': ' + lBuff);
    Result := TXplValue.Create(lBuff);
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
    lValue := GetVariable(lXVC.XplVariable);
    if (lValue = nil) then
      Continue; // can not find out variable value
    // compare with last from info
    lChanged:= (lXVC.LastValue = nil) or // first assignment
      (not lValue.Equals(lXVC.LastValue)); // different value
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
