unit uXplPluginEngine;

{$Define DEBUG}

interface

uses MemMap, XPLMDataAccess, uXplCommon, uXplSender, uXplPluginReceiver, uXplMessages, classes;

type

{ TXplEngine }

TXplEngine = class (TObject)
    fMM: TMemMap;
    fPosCountDown: Integer;
    fLatitudeRef: XPLMDataRef;
    fLongitudeRef: XPLMDataRef;
    fHeadingRef: XPLMDataRef;
    fHeightRef: XPLMDataRef;
    fDebugging: Boolean;
    pBuffer: PXplComRecord;
    fTextToBeDrawn: String;
    fScreenWidth: Integer;
    fScreenHeight: Integer;
    fBasicFontHeight: Integer;
    fBasicFontWidth: Integer;
    fTextFloatPosition: Single;
    fTextHideTs: TDateTime;
    fSender: TXplSender;
    fReceiver: TXplPluginReceiver;
    fDataRefs: TStrings;
    procedure DebugLog(Value: String);
    function GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
    procedure ProcessSlot(pSlot: PXplComSlot);
    procedure String2XplValue(pIn:String; pOut: PXplValue; pDataType: XPLMDataTypeID);
    procedure SimpleReadWrite(pSlot: PXplComSlot; pMakeChecks: Boolean);
    procedure ToggleVar(pSlot: PXplComSlot);
    procedure InitGlValues;
    procedure OnLmcMessage(pSender: TObject);
    procedure XplDebugFmt(pFormat:String; pArgs: array of const);
    procedure RunAndFree(pText: TXplDrawText); overload;
    procedure RunAndFree(pVar: TXplSetVariable); overload;
    function GetOrRegisterXplVariable(pName: String): TXplVariable;
    procedure SetVariable(pDef: TXplVariable; pVal: TXplValue);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure XplTick;
    procedure DrawText();
end;

implementation

uses SysUtils, XPLMUtilities, XPLMGraphics, gl, glu, XPLMDisplay, dateutils;

{ TXplEngine }

constructor TXplEngine.Create;
begin
  pBuffer := nil;
  fDebugging := FileExists('luamacros.log');
  fPosCountDown := -1;
  fTextToBeDrawn:='';
  fTextFloatPosition := 0;
  fMM := TMemMap.Create(XPL_MEM_FILE, SizeOf(TXplComRecord));
  if fMM.Memory <> nil then
  begin
    pBuffer := fMM.Memory;
    //DebugLog(Format('pBuffer addr is %s.', [IntToStr(ULong(pBuffer))]));
    //DebugLog(Format('Slot size: %d, mem size: %d', [SizeOf(TXplComSlot), SizeOf(TXplComRecord)]));
    if pBuffer^.HdmConnected > 0 then
    begin
      DebugLog('Hidmacros already connected to shared memory.');
      fPosCountDown := pBuffer^.PosInterval;
    end
    else
      DebugLog('Hidmacros not yet connected to shared memory.');
    pBuffer^.XplConnected := 1;
    pBuffer^.XplRequestFlag := 0;
  end
  else
    DebugLog('FATAL: Shared memory not created.');
  fSender := TXplSender.Create(cXplToLmcPipeName);
  fSender.DebugMethod:=XplDebugFmt;
  fReceiver := TXplPluginReceiver.Create(cLmcToXplPipeName);
  fReceiver.OnMessage:=OnLmcMessage;
  fReceiver.Init;
  fDataRefs:=TStringList.Create;
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
  if pBuffer <> nil then
    pBuffer^.XplConnected := 0;
  fMM.Free;
  fSender.Free;
  for I := 0 to fDataRefs.Count - 1 do
  begin
    fDataRefs.Objects[I].Free;
  end;
  fDataRefs.Free;
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
  i: Integer;
  lAllDone : Boolean;
begin
  //DebugLog('Tick');
  if pBuffer = nil then
    exit;
  if pBuffer^.XplRequestFlag = 1 then
  begin
    for I := 0 to COM_SLOTS_COUNT - 1 do
      if pBuffer^.ComSlots[i].XplRequestFlag = 1 then
      begin
        ProcessSlot(@pBuffer^.ComSlots[i]);
      end;
    lAllDone := True;
    for I := 0 to COM_SLOTS_COUNT - 1 do
      lAllDone := lAllDone and (pBuffer^.ComSlots[i].XplRequestFlag = 0);
    if lAllDone then
      pBuffer^.XplRequestFlag := 0;
  end;
  if (pBuffer^.HdmConnected > 0) and (fPosCountDown > 0) then
  begin
    Dec(fPosCountDown);
    if (fPosCountDown = 0) then
    begin
      // refresh pos values
      if fLatitudeRef = nil then
        fLatitudeRef := XPLMFindDataRef('sim/flightmodel/position/latitude');
      if fLongitudeRef = nil then
        fLongitudeRef := XPLMFindDataRef('sim/flightmodel/position/longitude');
      if fHeadingRef = nil then
        fHeadingRef := XPLMFindDataRef('sim/flightmodel/position/psi');
      if fHeightRef = nil then
        fHeightRef := XPLMFindDataRef('sim/flightmodel/position/elevation');
      if fLatitudeRef <> nil then
        pBuffer^.Latitude := XPLMGetDatad(fLatitudeRef);
      if fLongitudeRef <> nil then
        pBuffer^.Longitude := XPLMGetDatad(fLongitudeRef);
      if fHeadingRef <> nil then
        pBuffer^.Heading := XPLMGetDataf(fHeadingRef);
      if fHeightRef <> nil then
        pBuffer^.Height := XPLMGetDataf(fHeightRef);
      fPosCountDown := pBuffer^.PosInterval;
      //DebugLog('Pos: lat ' + FloatToStr(pBuffer^.Latitude) +
      //         ', lon ' + FloatToStr(pBuffer^.Longitude) +
      //         ', heading ' + FloatToStr(pBuffer^.Heading));
    end;
  end;
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
          SimpleReadWrite(pSlot, lMakeChecks);
        if (pSlot^.HDMcommand = HDMC_TOGGLE_NEXT) or
           (pSlot^.HDMcommand = HDMC_TOGGLE_PREVIOUS) or
           (pSlot^.HDMcommand = HDMC_SWITCH_NEXT) or
           (pSlot^.HDMcommand = HDMC_SWITCH_PREVIOUS) then
          ToggleVar(pSlot);
          end;
    end;
    if (pSlot^.HDMcommand = HDMC_EXEC_COMMAND) or
       (pSlot^.HDMcommand = HDMC_COMMAND_BEGIN) or
       (pSlot^.HDMcommand = HDMC_COMMAND_END)
    then
    begin
      fSender.SendString('Ahoj');
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
      fPosCountDown := pBuffer^.PosInterval;
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

procedure TXplEngine.SimpleReadWrite(pSlot: PXplComSlot; pMakeChecks: Boolean);
var
  lInt: Integer;
  lFloat: Single;
  lBuff: PChar;
begin
  case pSlot^.DataType of
  xplmType_Float:
    begin
      DebugLog('Got/set-ing value ' + FloatToStr(pSlot^.Value.floatData));
      case pSlot^.HDMcommand of
        HDMC_GET_VAR: pSlot^.Value.floatData := XPLMGetDataf(Pointer8b2Pointer(pSlot^.DataRef));
        HDMC_SET_VAR: XPLMSetDataf(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.Value.floatData);
      end;
      DebugLog('Got/set value ' + FloatToStr(pSlot^.Value.floatData));
    end;
  xplmType_Double:
    begin
      case pSlot^.HDMcommand of
        HDMC_GET_VAR: pSlot^.Value.doubleData := XPLMGetDatad(Pointer8b2Pointer(pSlot^.DataRef));
        HDMC_SET_VAR: XPLMSetDatad(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.Value.doubleData);
      end;
      DebugLog('Got/set value ' + FloatToStr(pSlot^.Value.doubleData));
    end;
  xplmType_Int:
    begin
      case pSlot^.HDMcommand of
        HDMC_GET_VAR: pSlot^.Value.intData := XPLMGetDatai(Pointer8b2Pointer(pSlot^.DataRef));
        HDMC_SET_VAR: XPLMSetDatai(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.Value.intData);
      end;
      DebugLog('Got/set value ' + IntToStr(pSlot^.Value.intData));
    end;
  xplmType_IntArray:
          begin
            // check range
            if pSlot^.Index < 0 then
              pSlot^.Value.intData := GetArrayLength(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.DataType)
      else if (not pMakeChecks) or (pSlot^.Index < (pSlot^.Length - 1)) then
            begin
              case pSlot^.HDMcommand of
                HDMC_GET_VAR:
                begin
                  XPLMGetDatavi(Pointer8b2Pointer(pSlot^.DataRef), @lInt,
                      pSlot^.Index, 1);
                  pSlot^.Value.intData := lInt;
                end;
                HDMC_SET_VAR:
                begin
                  lInt := pSlot^.Value.intData;
                  XPLMSetDatavi(Pointer8b2Pointer(pSlot^.DataRef), @lInt, pSlot^.Index, 1);
                end;
              end;
            DebugLog('Got/Set value ' + IntToStr(pSlot^.Value.intData));
            end else
              DebugLog('Index for int array too high ' + IntToStr(pSlot^.Index));
          end;
        xplmType_FloatArray:
          begin
            // check range
            if pSlot^.Index < 0 then
              pSlot^.Value.intData := GetArrayLength(Pointer8b2Pointer(pSlot^.DataRef), pSlot^.DataType)
      else if (not pMakeChecks) or (pSlot^.Index < (pSlot^.Length - 1)) then
            begin
              case pSlot^.HDMcommand of
                HDMC_GET_VAR:
                begin
                  XPLMGetDatavf(Pointer8b2Pointer(pSlot^.DataRef), @lFloat,
                      pSlot^.Index, 1);
                  pSlot^.Value.floatData := lFloat;
                end;
                HDMC_SET_VAR:
                begin
                  lFloat := pSlot^.Value.floatData;
                  XPLMSetDatavf(Pointer8b2Pointer(pSlot^.DataRef), @lFloat, pSlot^.Index, 1);
                end;
              end;
              DebugLog('Got/Set value ' + FloatToStr(pSlot^.Value.floatData));
            end else
              DebugLog('Index for array too high ' + IntToStr(pSlot^.Index));
          end;
        xplmType_Data:
          begin
            if (pSlot^.Length > XPL_MAX_STRING_SIZE) then
              pSlot^.Length := XPL_MAX_STRING_SIZE;
            lBuff := pSlot^.StringBuffer;
            case pSlot^.HDMcommand of
              HDMC_GET_VAR:
              begin
                XPLMGetDatab(Pointer8b2Pointer(pSlot^.DataRef), lBuff, 0, pSlot^.Length);
              end;
              HDMC_SET_VAR:
              begin
                lInt := StrLen(lBuff)+1; // copy also end str char
                XPLMSetDatab(Pointer8b2Pointer(pSlot^.DataRef), lBuff, 0, lInt);
              end;
            end;
            DebugLog('Got/Set value ' + lBuff);
          end;
        end;
      end;

procedure TXplEngine.ToggleVar(pSlot: PXplComSlot);
var
  lVals: array of TXplValueRec;
  lStrings: TStringList;
  i: Integer;
  lClosest: TXplValueRec;
  lClosestIndex : Integer;
  lCount: Integer;
  lSetIndex: Integer;
begin
  // step 1 - read current value
  case pSlot^.DataType of
  xplmType_Float:
    pSlot^.Value.floatData := XPLMGetDataf(Pointer8b2Pointer(pSlot^.DataRef));
  xplmType_Double:
    pSlot^.Value.doubleData := XPLMGetDatad(Pointer8b2Pointer(pSlot^.DataRef));
  xplmType_Int:
    pSlot^.Value.intData := XPLMGetDatai(Pointer8b2Pointer(pSlot^.DataRef));
  else
    exit;
  end;
  // step 2 - split incoming string to array
  lStrings:= TStringList.Create;
  try
    lStrings.Delimiter:=';';
    lStrings.DelimitedText:=pSlot^.ValueUntyped;
    if lStrings.Count = 0 then
      exit;
    lCount := lStrings.Count;
    SetLength(lVals, lCount);
    for i := 0 to lCount - 1 do
      String2XplValue(lStrings[i], @(lVals[i]), pSlot^.DataType);
  finally
    lStrings.Free;
  end;
  // step 3 - find the closest value
  lClosestIndex := 0;
  case pSlot^.DataType of
  xplmType_Float:  lClosest.floatData := Abs(lVals[lClosestIndex].floatData - pSlot^.Value.floatData);
  xplmType_Double: lClosest.doubleData := Abs(lVals[lClosestIndex].doubleData - pSlot^.Value.doubleData);
  xplmType_Int:    lClosest.intData := Abs(lVals[lClosestIndex].intData - pSlot^.Value.intData);
  end;
  for i := 0 to lCount - 1 do
    case pSlot^.DataType of
    xplmType_Float:
    begin
      if Abs(lVals[i].floatData - pSlot^.Value.floatData) < lClosest.floatData then
      begin
        lClosestIndex:=i;
        lClosest.floatData := Abs(lVals[i].floatData - pSlot^.Value.floatData);
      end;
      if lClosest.floatData = 0 then
        break;
    end;
    xplmType_Double:
    begin
      if Abs(lVals[i].doubleData - pSlot^.Value.doubleData) < lClosest.doubleData then
      begin
        lClosestIndex:=i;
        lClosest.doubleData := Abs(lVals[i].doubleData - pSlot^.Value.doubleData);
      end;
      if lClosest.doubleData = 0 then
        break;
    end;
    xplmType_Int:
    begin
      if Abs(lVals[i].intData - pSlot^.Value.intData) < lClosest.intData then
      begin
        lClosestIndex:=i;
        lClosest.intData := Abs(lVals[i].intData - pSlot^.Value.intData);
      end;
      if lClosest.intData = 0 then
        break;
    end;
  end;
  // step 4 - decide next value index
  case pSlot^.HDMcommand of
  HDMC_TOGGLE_NEXT:
    if (lClosestIndex + 1 = lCount) then
      lSetIndex:= 0
    else
      lSetIndex:= lClosestIndex + 1;
  HDMC_SWITCH_NEXT:
    if (lClosestIndex + 1 = lCount) then
      lSetIndex:= lClosestIndex
    else
      lSetIndex:= lClosestIndex + 1;
  HDMC_TOGGLE_PREVIOUS:
    if (lClosestIndex - 1 < 0) then
      lSetIndex:= lCount - 1
    else
      lSetIndex:= lClosestIndex - 1;
  HDMC_SWITCH_PREVIOUS:
    if (lClosestIndex - 1 < 0) then
      lSetIndex:= 0
    else
      lSetIndex:= lClosestIndex - 1;
  end;
  DebugLog(Format('Toggle: The closest index is %d, so setting %d in values [%s].', [lClosestIndex, lSetIndex, pSlot^.ValueUntyped]));
  // step 5 - set value
  case pSlot^.DataType of
  xplmType_Float:  XPLMSetDataf(Pointer8b2Pointer(pSlot^.DataRef), lVals[lSetIndex].floatData);
  xplmType_Double: XPLMSetDatad(Pointer8b2Pointer(pSlot^.DataRef), lVals[lSetIndex].doubleData);
  xplmType_Int:    XPLMSetDatai(Pointer8b2Pointer(pSlot^.DataRef), lVals[lSetIndex].intData);
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
      DebugLog('Variable ' + pVar.Name + ' is not witable.');
  end;
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
