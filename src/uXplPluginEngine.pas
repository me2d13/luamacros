unit uXplPluginEngine;

{$Define DEBUG}

interface

uses MemMap, XPLMDataAccess, uXplCommon;

type

TXplEngine = class (TObject)
    fMM: TMemMap;
    fPosCountDown: Integer;
    fLatitudeRef: XPLMDataRef;
    fLongitudeRef: XPLMDataRef;
    fHeadingRef: XPLMDataRef;
    fHeightRef: XPLMDataRef;
    pBuffer: PXplComRecord;
    procedure DebugLog(Value: String);
    function GetArrayLength(pDataRef: XPLMDataRef ;pDataType: XPLMDataTypeID): Integer;
    procedure ProcessSlot(pSlot: PXplComSlot);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure XplTick;
end;

implementation

uses SysUtils, Windows, XPLMUtilities;

{ TXplEngine }

constructor TXplEngine.Create;
begin
  pBuffer := nil;
  fPosCountDown := -1;
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
end;

procedure TXplEngine.DebugLog(Value: String);
var
  tmp: PChar;
  lVal: String;
  logFile: TextFile;
begin
  {$IfNDef DEBUG}
  if pBuffer = nil then
    exit;
  if not pBuffer^.Debug then
    exit;
  {$Endif}
  lVal := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now) + ' : '+ Value;
  {$IFDEF OUTPUTDEBUGSTRING}
  GetMem(tmp, Length(lVal) + 1);
  try
    StrPCopy(tmp, lVal);
    OutputDebugString(tmp);
  finally
    FreeMem(tmp);
  end;
  {$ELSE}
  // to file
  AssignFile(logFile, 'debug.log');
  if FileExists('debug.log') then
      Append(logFile)
    else
      Rewrite(logFile);
  WriteLn(logFile, lVal);
  CloseFile(logFile);
  {$ENDIF}
end;

destructor TXplEngine.Destroy;
begin
  if pBuffer <> nil then
    pBuffer^.XplConnected := 0;
  fMM.Free;
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
        DebugLog('Processing slot ' + IntToStr(I));
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
    DebugLog(MemoryDump(pSlot, 1570));
    // check command
    if (pSlot^.HDMcommand = HDMC_GET_VAR) or (pSlot^.HDMcommand = HDMC_SET_VAR) then
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
          // convert from string to appropriate value
          case pSlot^.DataType of
          xplmType_Float,
          xplmType_FloatArray:
            pSlot^.Value.floatData := StrToFloat(pSlot^.ValueUntyped);
          xplmType_Double:
            pSlot^.Value.doubleData := StrToFloat(pSlot^.ValueUntyped);
          xplmType_Int,
          xplmType_IntArray:
            pSlot^.Value.intData := StrToint(pSlot^.ValueUntyped);
          end;
        end;
        if lMakeChecks and (pSlot^.HDMcommand = HDMC_SET_VAR) and (not pSlot^.Writable) then
        begin
          DebugLog('Can''t set variable which is read only, chenging to get.');
          pSlot^.HDMcommand := HDMC_GET_VAR;
        end;
        case pSlot^.DataType of
        xplmType_Float:
          begin
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
            else if (not lMakeChecks) or (pSlot^.Index < (pSlot^.Length - 1)) then
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
            else if (not lMakeChecks) or (pSlot^.Index < (pSlot^.Length - 1)) then
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
        //SetString(lString, PChar(@(pSlot^.ValueName[0])), Length(pSlot^.ValueName));
        //DebugLog('Finding ref for converted command ' + pSlot^.ValueName);
        //DebugLog(MemoryDump(pSlot, 1570));
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
    pSlot^.XplRequestFlag := 0;
  end;
end;

end.
