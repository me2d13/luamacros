unit uXplPluginEngine;

{$Define DEBUG}

interface

uses XPLMDataAccess, XPLMUtilities, uXplCommon, uXplSender, uXplPluginReceiver, uXplMessages, classes, fgl, uXplLogger, MemMap;

type

TCallbackInfoMap = TFPGMap<Int64,TXplVariableCallbackInfo>;

{ TXplEngine }

TXplEngine = class (TObject)
    fLogger: TXplLogger;
    fDebugLogFileName: String;
    fTextToBeDrawn: String;
    fScreenWidth: Integer;
    fScreenHeight: Integer;
    fBasicFontHeight: Integer;
    fBasicFontWidth: Integer;
    fTextFloatPosition: Single;
    fTextHideTs: TDateTime;
    fDataRefs: TStringList;
    fCommands: TStringList;
    fWatchedVariables: TStringList;
    fLmc2XplMem: TMemMap;
    fXpl2LmcMem: TMemMap;
    fXplMem: PXpl2LmcSharedMem;
    fLmcMem: PLmc2XplSharedMem;
    fLastProcessedId: Int64;
    fMaxComIdInTick: Int64;
    fTickNo: Int64;
    procedure DebugLog(Value: String);
    procedure DebugLogFmt(pFormat:String; pArgs: array of const);
    function GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
    procedure String2XplValue(pIn:String; pOut: PXplValue; pDataType: XPLMDataTypeID);
    procedure InitGlValues;
    procedure XplDebugFmt(pFormat:String; pArgs: array of const);
    procedure Run(pText: TXplDrawTextRec); overload;
    procedure Run(pData: TXplVariableWithValueRec); overload;
    procedure Run(pData: TXplIncVariableRec); overload;
    procedure Run(pData: TXplGetVariableRequestRec); overload;
    procedure RunAndFree(pVar: TXplGetVariable); overload;
    procedure Run(pComm: TXplCommandRec);
    procedure RunAndFree(pVar: TXplLogCommand); overload;
    procedure RunAndFree(pVar: TXplIncVariable); overload;
    function GetOrRegisterXplVariable(pName: String): TXplVariable;
    function GetOrRegisterXplCommand(pName: String): XPLMCommandRef;
    procedure SetVariable(pDef: TXplVariable; pVal: TXplValue; pIndex: Int64);
    function GetVariable(pDef: TXplVariable; pProduceLog: Boolean): TXplValue; overload;
    function GetVariable(pDef: TXplVariable; pIndex: Integer; pProduceLog: Boolean): TXplValue; overload;
    function UnixTimestampMs: Int64;
    function ValidLmcRequest(header: TComSlotRec): Boolean;
    procedure CheckCommandQueue;
    procedure LmcStarted;
    procedure UpdateWatchedValue(pIndex: Integer);
    function WatchedKey(lRec: PXplGetVariableRequestRec): String;
    procedure RefreshWatchedVariables;
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
  fTickNo := 0;
  fLogger := TXplLogger.Create;
  fXpl2LmcMem := TMemMap.Create(cXpl2LmcMemName, SizeOf(TXpl2LmcSharedMem), True);
  fXplMem:=fXpl2LmcMem.Memory;
  fLmc2XplMem := TMemMap.Create(cLmc2XplMemName, SizeOf(TLmc2XplSharedMem), True);
  fLmcMem:=fLmc2XplMem.Memory;
  fLastProcessedId:=0;
  fTextToBeDrawn:='';
  fTextFloatPosition := 0;
  DebugLog('Going to init internal structures...');
  fDataRefs:=TStringList.Create;
  fDataRefs.CaseSensitive:=False;
  fCommands:=TStringList.Create;
  fCommands.CaseSensitive:=False;
  fWatchedVariables:=TStringList.Create;
  fWatchedVariables.CaseSensitive:=False;
  InitGlValues;
  DebugLog('LuaMacros init done');
  //DebugLogFmt('Size of TLmc2XplSharedMem is %d', [SizeOf(TLmc2XplSharedMem)]);
end;

procedure TXplEngine.DebugLog(Value: String);
begin
  fLogger.Log(Value);
end;

procedure TXplEngine.DebugLogFmt(pFormat: String; pArgs: array of const);
begin
  fLogger.LogFmt(pFormat, pArgs);
end;

destructor TXplEngine.Destroy;
var
  I: Integer;
begin
  fLmc2XplMem.Free;
  fXpl2LmcMem.Free;
  for I := 0 to fDataRefs.Count - 1 do
  begin
    fDataRefs.Objects[I].Free;
  end;
  for I := 0 to fWatchedVariables.Count - 1 do
  begin
    fWatchedVariables.Objects[I].Free;
  end;
  fDataRefs.Free;
  fCommands.Free;
  fWatchedVariables.Free;
  fLogger.Free;
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
var
  lOrigLastId: Int64;
begin
  Inc(fTickNo);
  lOrigLastId := fLastProcessedId;
  fMaxComIdInTick:=0;
  CheckCommandQueue;
  RefreshWatchedVariables;
  if (fLastProcessedId < fMaxComIdInTick) then
  begin
    fLastProcessedId:=fMaxComIdInTick;
  end;
  if fLastProcessedId <> lOrigLastId then
    DebugLogFmt('After xpl tick %d we have fLastProcessedId = %d, fMaxComIdInTick = %d', [fTickNo, fLastProcessedId, fMaxComIdInTick]);
  fXplMem^.LastProcessedId:=fLastProcessedId;
  fXplMem^.Lock:=LOCK_NONE;
  fXplMem^.UpdateTimeStamp:=UnixTimestampMs;
end;

procedure TXplEngine.CheckCommandQueue;
var
  lIndex: Integer;
begin
  for lIndex := Low(fLmcMem^.Commands) to High(fLmcMem^.Commands) do
  begin
    if ValidLmcRequest(fLmcMem^.Commands[lIndex].Header) then
    begin
      DebugLogFmt('Tick %d: Detected valid command index %d, id %d type %d', [fTickNo, lIndex, fLmcMem^.Commands[lIndex].Header.Id, fLmcMem^.Commands[lIndex].CommandType]);
      case fLmcMem^.Commands[lIndex].CommandType of
      LMC_COMMAND_LMC_STARTED:
          LmcStarted;
      LMC_COMMAND_DRAW_TEXT:
        Run(fLmcMem^.Commands[lIndex].TextData);
      LMC_COMMAND_XPL_COMMAND:
        Run(fLmcMem^.Commands[lIndex].CommandData);
      LMC_COMMAND_SET_VARIABLE:
        Run(fLmcMem^.Commands[lIndex].SetVariableData);
      LMC_COMMAND_INC_VARIABLE:
        Run(fLmcMem^.Commands[lIndex].IncVariableData);
      LMC_COMMAND_GET_VARIABLE:
        Run(fLmcMem^.Commands[lIndex].GetVariableData);
      end;
      DebugLogFmt('Tick %d: After slot process we have fLastProcessedId = %d, fMaxComIdInTick = %d', [fTickNo, fLastProcessedId, fMaxComIdInTick]);
    end;
  end;
end;

procedure TXplEngine.LmcStarted;
begin
  DebugLog('Processing Lmc started command');
  fLastProcessedId:=0;
  fMaxComIdInTick:=0;
end;

procedure TXplEngine.UpdateWatchedValue(pIndex: Integer);
var
  lValue: TXplValue;
  lXplVariable: TXplVariable;
  lNow : Int64;
begin
  if (pIndex >= High(TXpl2LmcSharedMem.Values)) then
  begin
    DebugLogFmt('Cannot send more values to LuaMacros. Maximum %d reached.', [High(TXpl2LmcSharedMem.Values)]);
  end else begin
    lXplVariable := TXplVariable(fWatchedVariables.Objects[pIndex]);
    lValue := GetVariable(lXplVariable, fXplMem.Values[pIndex].Value.Index, True);
    if (lValue <> nil) then
    begin
      lNow:=UnixTimestampMs;
      fXplMem.Values[pIndex].Header.Status:=LMC_STATUS_WRITING;
      fXplMem.Values[pIndex].Header.TimeStamp:=lNow;
      fXplMem.Values[pIndex].Value.Value := lValue.Value;
      fXplMem.Values[pIndex].Header.Status:=LMC_STATUS_READY;
    end
    else
      DebugLog('Can not find out value of variable ' + lXplVariable.Name);
  end;
end;

function TXplEngine.WatchedKey(lRec: PXplGetVariableRequestRec): String;
begin
  Result := Format('%s::%d', [lRec^.Name, lRec^.Index]);
end;

procedure TXplEngine.RefreshWatchedVariables;
var
  I: Integer;
begin
  for I := 0 to fWatchedVariables.Count -1 do
    UpdateWatchedValue(I);
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
      pOut^.doubleData := StrToFloat(pIn, FS);
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

procedure TXplEngine.Run(pText: TXplDrawTextRec);
begin
  fTextFloatPosition := pText.Position;
  fTextToBeDrawn := pText.Value;
  if (pText.TimeInSec > 0) then
    fTextHideTs := IncSecond(Now(), pText.TimeInSec)
  else
    fTextHideTs := 0;
  DebugLog(Format('Received DrawText %s at pos %f.', [fTextToBeDrawn, fTextFloatPosition]));
end;

procedure TXplEngine.Run(pData: TXplVariableWithValueRec);
var
  lXV: TXplVariable;
  lXplValue: TXplValue;
begin
  lXplValue := TXplValue.Create(pData.Value);
  if (pData.Index = NO_INDEX) then
    DebugLog('Received request to set variable ' + pData.Name + ' to ' + lXplValue.ToString)
  else
    DebugLogFmt('Received request to set variable %s[%d] to %s', [pData.Name, pData.Index, lXplValue.ToString]);
  lXV := GetOrRegisterXplVariable(pData.Name);
  if (lXV <> nil) then
  begin
    if (lXV.Writable) then
    begin
      DebugLog('Variable ' + pData.Name + ' is writable, setting...');
      SetVariable(lXV, lXplValue, pData.Index)
    end
    else
      DebugLog('Variable ' + pData.Name + ' is not writable.');
  end
  else
    DebugLog('Cannot set variable ' + pData.Name + ' - not found.');
  lXplValue.Free;
end;

procedure TXplEngine.Run(pData: TXplIncVariableRec);
var
  lObject: TXplIncVariable;
begin
  lObject := TXplIncVariable.Create(pData);
  RunAndFree(lObject);
end;

procedure TXplEngine.Run(pData: TXplGetVariableRequestRec);
var
  lIndex: Integer;
  lVariable: TXplVariable;
begin
  lIndex := fWatchedVariables.IndexOf(WatchedKey(@pData));
  if (lIndex < 0) then
  begin
    // not found, add to list
    lVariable := GetOrRegisterXplVariable(pData.Name);;
    if (lVariable <> nil) then
    begin
      fXplMem.Lock:=LOCK_LIST_CHANGING;
      fWatchedVariables.AddObject(WatchedKey(@pData), lVariable);
      lIndex:=fWatchedVariables.Count-1;
      fXplMem.Values[lIndex].Header.Status:=LMC_STATUS_WRITING;
      fXplMem.Values[lIndex].Value.Name:=pData.Name;
      fXplMem.Values[lIndex].Value.Index:=pData.Index;
      UpdateWatchedValue(lIndex);
      fXplMem.Lock:=LOCK_NONE;
    end;
  end
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
      //fSyncSender.SendMessage(TXplVariableValue.Create(pVar.Name, lValue, pVar.Id));
      DebugLog('Written to stream');
    end
    else
      DebugLog('Can not find out value of variable ' + pVar.Name);
  end
  else
    DebugLog('Variable ' + pVar.Name + ' not found.');
  pVar.Free;
end;

procedure TXplEngine.Run(pComm: TXplCommandRec);
var
  lCom: XPLMCommandRef;
begin
  lCom:=GetOrRegisterXplCommand(pComm.Name);
  if (lCom <> nil) then
  begin
    if (pComm.Mode = XPL_COMMAND_START) then
    begin
      XPLMCommandBegin(lCom);
      DebugLog(Format('Executed command begin %s.', [pComm.Name]));
    end else
    if (pComm.Mode = XPL_COMMAND_END) then
    begin
      XPLMCommandEnd(lCom);
      DebugLog(Format('Executed command end %s.', [pComm.Name]));
    end else
    begin
      XPLMCommandOnce(lCom);
      DebugLog(Format('Executed command %s.', [pComm.Name]));
    end;
  end;
end;

procedure TXplEngine.RunAndFree(pVar: TXplLogCommand);
begin
  if (pVar.Name = cLogCommandLogFileName) then
  begin
    fLogger.LogLevel:=cLlDebug;
    fLogger.DebugLogFileName:=pVar.Value;
    DebugLogFmt('Received log file name %s', [fLogger.DebugLogFileName]);
  end
  else
    DebugLogFmt('Unknown log command %s = %s', [pVar.Name, pVar.Value]);
  pVar.Free;
end;

procedure TXplEngine.RunAndFree(pVar: TXplIncVariable);
var
  lXV: TXplVariable;
  lValue: TXplValue;
  lIntLimit: Int64;
  lIntOverflowBase: Int64;
begin
  DebugLog(Format('Received request %s.', [pVar.ToString]));
  lXV := GetOrRegisterXplVariable(pVar.Name);
  if (lXV <> nil) then
  begin
    if (lXV.Writable) then
    begin
      lValue := GetVariable(lXV, pVar.Index, True);
      if (lValue <> nil) then
      begin
        if (lValue.ValueType = vtInteger) then
        begin
          pVar.Value.MakeInt;
          DebugLogFmt('Curent int value is %d, delta is %d', [lValue.IntValue, pVar.Value.IntValue]);
          lValue.IntValue:=lValue.IntValue + pVar.Value.IntValue;
          DebugLogFmt('After inc Curent int value is %d, delta is %d', [lValue.IntValue, pVar.Value.IntValue]);
          if (pVar.HasLimit) then
          begin
            DebugLog('Has limit');
            lIntLimit:=trunc(pVar.Limit);
            lIntOverflowBase:=trunc(pVar.OverflowBase);
            if (pVar.Value.IntValue > 0) and (lValue.IntValue > lIntLimit) then
              if pVar.UseOverflow then
                lValue.IntValue := lIntOverflowBase + (lValue.IntValue - lIntLimit)
              else
                lValue.IntValue := lIntLimit
            else if (pVar.Value.IntValue < 0) and (lValue.IntValue < lIntLimit) then
              if pVar.UseOverflow then
                lValue.IntValue := lIntOverflowBase - (lIntLimit - lValue.IntValue)
              else
                lValue.IntValue := lIntLimit;
          end;
          DebugLogFmt('Variable %s is writable, setting to %d', [pVar.Name, lValue.IntValue]);
          SetVariable(lXV, lValue, pVar.Index)
        end
        else if (lValue.ValueType = vtDouble) then
        begin
          pVar.Value.MakeDouble;
          lValue.DoubleValue:=lValue.DoubleValue + pVar.Value.DoubleValue;
          if (pVar.HasLimit) then
            if (pVar.Value.DoubleValue > 0) and (lValue.DoubleValue > pVar.Limit) then
              if pVar.UseOverflow then
                lValue.DoubleValue := pVar.OverflowBase + (lValue.DoubleValue - pVar.Limit)
              else
                lValue.DoubleValue := pVar.Limit
            else if (pVar.Value.DoubleValue < 0) and (lValue.DoubleValue < pVar.Limit) then
              if pVar.UseOverflow then
                lValue.DoubleValue := pVar.OverflowBase - (pVar.Limit - lValue.DoubleValue)
              else
                lValue.DoubleValue := pVar.Limit;
          DebugLogFmt('Variable %s is writable, setting to %f', [pVar.Name, lValue.DoubleValue]);
          SetVariable(lXV, lValue, pVar.Index)
        end
        else
          DebugLog('Wrong type of variable. Only int and float tyoes can be increased');
      end
      else
        DebugLog('Can not find out value of variable ' + pVar.Name);
    end
    else
      DebugLog('Variable ' + pVar.Name + ' is not writable.');
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

function TXplEngine.UnixTimestampMs: Int64;
begin
  //Result := Round(Now * 24*60*60*1000);
  Result := Round((Now - 25569) * 86400*1000);
end;

function TXplEngine.ValidLmcRequest(header: TComSlotRec): Boolean;
begin
  Result := (header.Status = LMC_STATUS_READY)
    and (UnixTimestampMs - header.TimeStamp < cMaxAcceptableRequestDelayMs)
    and (header.Id > fLastProcessedId);
  if Result and (fMaxComIdInTick < header.Id) then
  begin
    fMaxComIdInTick := header.Id;
  end;
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
