unit uXplPluginEngine;

{$Define DEBUG}

interface

uses XPLMDataAccess, XPLMUtilities, uXplCommon, uXplSender, uXplPluginReceiver, uXplMessages, classes;

type

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
    fSender: TXplSender;
    fReceiver: TXplPluginReceiver;
    fDataRefs: TStringList;
    fCommands: TStringList;
    procedure DebugLog(Value: String);
    function GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
    procedure ProcessSlot(pSlot: PXplComSlot);
    procedure String2XplValue(pIn:String; pOut: PXplValue; pDataType: XPLMDataTypeID);
    procedure InitGlValues;
    procedure OnLmcMessage(pSender: TObject);
    procedure XplDebugFmt(pFormat:String; pArgs: array of const);
    procedure RunAndFree(pText: TXplDrawText); overload;
    procedure RunAndFree(pVar: TXplSetVariable); overload;
    procedure RunAndFree(pVar: TXplGetVariable); overload;
    procedure RunAndFree(pComm: TXplExecuteCommand); overload;
    function GetOrRegisterXplVariable(pName: String): TXplVariable;
    function GetOrRegisterXplCommand(pName: String): XPLMCommandRef;
    procedure SetVariable(pDef: TXplVariable; pVal: TXplValue);
    function GetVariable(pDef: TXplVariable): TXplValue;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure XplTick;
    procedure DrawText();
end;

implementation

uses SysUtils, XPLMGraphics, gl, glu, XPLMDisplay, dateutils;

{ TXplEngine }

constructor TXplEngine.Create;
begin
  fDebugging := FileExists('luamacros.log');
  fTextToBeDrawn:='';
  fTextFloatPosition := 0;
  fSender := TXplSender.Create(cXplToLmcPipeName);
  fSender.DebugMethod:=XplDebugFmt;
  fReceiver := TXplPluginReceiver.Create(cLmcToXplPipeName);
  fReceiver.OnMessage:=OnLmcMessage;
  fReceiver.Init;
  fDataRefs:=TStringList.Create;
  fDataRefs.CaseSensitive:=False;
  fCommands:=TStringList.Create;
  fCommands.CaseSensitive:=False;
  if (fSender.ServerRunning) then
  begin
    // LMC is already running, but its sender could be connected to nowhere (old XPL instance)
    // send it reconnect request
    fSender.SendMessage(TXplReconnectToServer.Create);
  end;
  InitGlValues;
end;

procedure TXplEngine.DebugLog(Value: String);
var
  tmp: PChar;
  lVal: String;
  logFile: TextFile;
begin
  if not fDebugging then
    exit;
  lVal := 'XPLLUMplugin:'+ Value;
  {$IFDEF OUTPUTDEBUGSTRING}
  GetMem(tmp, Length(lVal) + 1);
  try
    StrPCopy(tmp, lVal);
    OutputDebugString(tmp);
  finally
    FreeMem(tmp);
  end;
  {$ENDIF}
  // to file
  AssignFile(logFile, 'luamacros.log');
  if FileExists('luamacros.log') then
      Append(logFile)
    else
      Rewrite(logFile);
  WriteLn(logFile, lVal);
  CloseFile(logFile);
end;

destructor TXplEngine.Destroy;
var
  I: Integer;
begin
  fReceiver.Free;
  fSender.Free;
  for I := 0 to fDataRefs.Count - 1 do
  begin
    fDataRefs.Objects[I].Free;
  end;
  fDataRefs.Free;
  fCommands.Free;
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
  DrawText();
end;

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
  lMessageType, b, i: byte;
  lStr: String;
  lInt: Int64;
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
        HDMC_RECONNECT: fSender.Reconnect;
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
      fSender.SendMessage(TXplVariableValue.Create(pVar.Name, lValue, pVar.Id));
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
    DebugLog(Format('Got float value %f of variable %s.', [lSingle, pDef.Name]));
    Result := TXplValue.Create(lSingle);
  end;
  xplmType_Double:
  begin
    lReal:=XPLMGetDatad(pDef.DataRef);
    DebugLog(Format('Got double value %f of variable %s.', [lReal, pDef.Name]));
    Result := TXplValue.Create(lReal);
  end;
  xplmType_Int:
  begin
    lInt:=XPLMGetDatai(pDef.DataRef);
    DebugLog(Format('Got int value %d of variable %s.', [lInt, pDef.Name]));
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
    DebugLog('Got string value of variable ' + pDef.Name + ': ' + lBuff);
    Result := TXplValue.Create(lBuff);
  end;
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
