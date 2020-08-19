unit uSendKeys;

interface

Uses SysUtils, Windows, Messages, Classes;

const
  ShiftsVK : array [0..14] of byte =
    (VK_CONTROL, VK_MENU, VK_SHIFT, VK_LWIN, VK_TAB,
     VK_LCONTROL, VK_LMENU, VK_LSHIFT, VK_LWIN, VK_TAB,
     VK_RCONTROL, VK_RMENU, VK_RSHIFT, VK_RWIN, VK_TAB);

  cKeyLogger = 'KEY';

type
  TKeySequence = class (TThread)
  private
    fSequence: String;
    fDelayModifiers: Integer;
    fDelayKeys: Integer;
    fShiftsDown: array [0..High(ShiftsVK)] of Boolean; // ctrl, alt, shift, lefts, rights
    function BitSet(BitTable, BitMask : Byte) : Boolean;
    procedure SetBit(var BitTable : Byte; BitMask : Byte);
    procedure DebugLog(Value: String);
    procedure KeyboardEvent(VKey, ScanCode : Byte; Flags : Longint); // calls winAPI keybd_event
    procedure SendKeyDown(VKey: Byte; NumTimes : Word; GenUpMsg : Boolean); // calls KeyboardEvent
    procedure SendKeyUp(VKey: Byte); // calls KeyboardEvent
    procedure SendKey(MKey: Word; NumTimes : Word;
        GenDownMsg : Boolean); // calls SendKeyDown and SendKeyUp
    function StringToVKey(KeyString : ShortString) : Word; // return vkey code
    procedure PopUpShiftKeys; // popup all modifiers
    procedure SendKeys; // the method
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; Override;
    property Sequence: String read fSequence write fSequence;
    property DelayModifiers: Integer read fDelayModifiers write fDelayModifiers;
    property DelayKeys: Integer read fDelayKeys write fDelayKeys;
  end;

procedure SendKeyboardInput(pVk, pScan, pFlags: Integer);
procedure SendUnicodeInput(pCode, pDirection: Integer);

implementation

uses uGlobals, JwaWinUser, uDevice, character;

type
  TSendKey = record
    Name : ShortString;
    VKey : Byte;
  end;

const
  {Array of keys that SendKeys recognizes.

  If you add to this list, you must be sure to keep it sorted alphabetically
  by Name because a binary search routine is used to scan it.}

  MaxSendKeyRecs = 66;
  SendKeyRecs : array[1..MaxSendKeyRecs] of TSendKey =
  (
   (Name:'BKSP';            VKey:VK_BACK),
   (Name:'BS';              VKey:VK_BACK),
   (Name:'BACKSPACE';       VKey:VK_BACK),
   (Name:'BREAK';           VKey:VK_CANCEL),
   (Name:'CAPSLOCK';        VKey:VK_CAPITAL),
   (Name:'CLEAR';           VKey:VK_CLEAR),
   (Name:'DEL';             VKey:VK_DELETE),
   (Name:'DELETE';          VKey:VK_DELETE),
   (Name:'DOWN';            VKey:VK_DOWN),
   (Name:'END';             VKey:VK_END),
   (Name:'ENTER';           VKey:VK_RETURN),
   (Name:'ESC';             VKey:VK_ESCAPE),
   (Name:'ESCAPE';          VKey:VK_ESCAPE),
   (Name:'F1';              VKey:VK_F1),
   (Name:'F10';             VKey:VK_F10),
   (Name:'F11';             VKey:VK_F11),
   (Name:'F12';             VKey:VK_F12),
   (Name:'F13';             VKey:VK_F13),
   (Name:'F14';             VKey:VK_F14),
   (Name:'F15';             VKey:VK_F15),
   (Name:'F16';             VKey:VK_F16),
   (Name:'F17';             VKey:VK_F17),
   (Name:'F18';             VKey:VK_F18),
   (Name:'F19';             VKey:VK_F19),
   (Name:'F2';              VKey:VK_F2),
   (Name:'F20';             VKey:VK_F20),
   (Name:'F21';             VKey:VK_F21),
   (Name:'F22';             VKey:VK_F22),
   (Name:'F23';             VKey:VK_F23),
   (Name:'F24';             VKey:VK_F24),
   (Name:'F3';              VKey:VK_F3),
   (Name:'F4';              VKey:VK_F4),
   (Name:'F5';              VKey:VK_F5),
   (Name:'F6';              VKey:VK_F6),
   (Name:'F7';              VKey:VK_F7),
   (Name:'F8';              VKey:VK_F8),
   (Name:'F9';              VKey:VK_F9),
   (Name:'HELP';            VKey:VK_HELP),
   (Name:'HOME';            VKey:VK_HOME),
   (Name:'INS';             VKey:VK_INSERT),
   (Name:'LEFT';            VKey:VK_LEFT),
   (Name:'LWIN';            VKey:VK_LWIN),
   (Name:'NUM0';            VKey:VK_NUMPAD0),
   (Name:'NUM1';            VKey:VK_NUMPAD1),
   (Name:'NUM2';            VKey:VK_NUMPAD2),
   (Name:'NUM3';            VKey:VK_NUMPAD3),
   (Name:'NUM4';            VKey:VK_NUMPAD4),
   (Name:'NUM5';            VKey:VK_NUMPAD5),
   (Name:'NUM6';            VKey:VK_NUMPAD6),
   (Name:'NUM7';            VKey:VK_NUMPAD7),
   (Name:'NUM8';            VKey:VK_NUMPAD8),
   (Name:'NUM9';            VKey:VK_NUMPAD9),
   (Name:'NUMDECIMAL';      VKey:VK_DECIMAL),
   (Name:'NUMDIVIDE';       VKey:VK_DIVIDE),
   (Name:'NUMLOCK';         VKey:VK_NUMLOCK),
   (Name:'NUMMINUS';        VKey:VK_SUBTRACT),
   (Name:'NUMMULTIPLY';     VKey:VK_MULTIPLY),
   (Name:'NUMPLUS';         VKey:VK_ADD),
   (Name:'PGDN';            VKey:VK_NEXT),
   (Name:'PGUP';            VKey:VK_PRIOR),
   (Name:'PRTSC';           VKey:VK_PRINT),
   (Name:'RIGHT';           VKey:VK_RIGHT),
   (Name:'RWIN';            VKey:VK_RWIN),
   (Name:'SCROLLLOCK';      VKey:VK_SCROLL),
   (Name:'TAB';             VKey:VK_TAB),
   (Name:'UP';              VKey:VK_UP)
  );

  {Extra VK constants missing from Delphi's Windows API interface}
  VK_NULL=0;
  VK_SemiColon=186;
  VK_Equal=187;
  VK_Comma=188;
  VK_Minus=189;
  VK_Period=190;
  VK_Slash=191;
  VK_BackQuote=192;
  VK_LeftBracket=219;
  VK_BackSlash=220;
  VK_RightBracket=221;
  VK_Quote=222;
  VK_Last=VK_Quote;

  ExtendedVKeys : set of byte =
  [VK_Up,
   VK_Down,
   VK_Left,
   VK_Right,
   VK_Home,
   VK_End,
   VK_Prior,  {PgUp}
   VK_Next,   {PgDn}
   VK_Insert,
   VK_Delete];


const
  INVALIDKEY = $FFFF {Unsigned -1};
  VKKEYSCANSHIFTON = $01;
  VKKEYSCANCTRLON = $02;
  VKKEYSCANALTON = $04;

procedure SendKeyboardInput(pVk, pScan, pFlags: Integer);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input.type_ := INPUT_KEYBOARD;
  Input.ki.dwFlags := pFlags;
  Input.ki.wVk := pVk;
  Input.ki.wScan:=pScan;
  SendInput(1, @Input, SizeOf(Input));
end;

procedure SendUnicodeInput(pCode, pDirection: Integer);
var
  lInput: packed array[0..4] of TInput;
  lFlags: Integer;
  I: Integer;
  lCount: Integer;
  lStr: UnicodeString;
begin
  if (pDirection = cDirectionDown) then lFlags:=4;
  if (pDirection = cDirectionUp) then lFlags:=6;
  FillChar(lInput, SizeOf(Input)*5, 0);
  if (lFlags > 0) and (pCode <= 65535) then
  begin
    lInput[0].type_ := INPUT_KEYBOARD;
    lInput[0].ki.dwFlags := lFlags;
    lInput[0].ki.wVk := 0;
    lInput[0].ki.wScan:=pCode;
    SendInput(1, @lInput[0], SizeOf(TInput));
  end;
  if (lFlags > 0) and (pCode >= $010000) and (pCode <= $10FFFF) then
  begin
    lStr := ConvertFromUtf32(pCode);
    lCount := Length(lStr);
    lCount := 2;
    for I := 0 to lCount - 1 do
    begin
      lInput[I].type_ := INPUT_KEYBOARD;
      lInput[I].ki.dwFlags := lFlags;
      lInput[I].ki.wVk := 0;
      lInput[I].ki.wScan:=Word(lStr[I]);
    end;
    lInput[0].ki.wScan:=$D83D;
    lInput[1].ki.wScan:=$DE04;
    SendInput(lCount, lInput, SizeOf(TInput));
  end;
end;

constructor TKeySequence.Create;
begin
  // Create suspended:
  inherited Create(True);
  Self.FreeOnTerminate:=True;
  fDelayModifiers := 0;
  fDelayKeys := 0;
end;

destructor TKeySequence.Destroy;
begin
  Self.FreeOnTerminate:=False;
  Self.Terminate;
  inherited Destroy;
end;

procedure TKeySequence.Execute;
begin
  try
    SendKeys;
  except
    on E: Exception do
      Glb.LogError(E.Message, cKeyLogger);
  end;
end;
  
procedure TKeySequence.SendKeys;
//Function SendKeys(SendKeysString : PChar; Wait : Boolean; ToWindow : string = ''; Delayms: Integer = 0) : Boolean;
type
  WBytes = array[0..pred(SizeOf(Word))] of Byte;
var
  UsingParens, FoundClose : Boolean;
  PosSpace : Byte;
  I, L, ShiftIndex : Integer;
  NumTimes, MKey : Word;
  KeyString : String[20];
  TmpString: String;
  AllocationSize : integer;
begin
  AllocationSize:=MaxInt;
  UsingParens:=false;
  for I := Low(fShiftsDown) to High(fShiftsDown) do
    fShiftsDown[I] := False;
  I:=1;
  L:=Length(fSequence);
  If (L>AllocationSize) then L:=AllocationSize;
  If (L=0) then Exit;

  While (I<=L) do begin
    // find out if there's < or > after shift
    ShiftIndex := 0;
    if I+1 < L then
    begin
      if fSequence[I+1] = '<' then
        ShiftIndex := 5;
      if fSequence[I+1] = '>' then
        ShiftIndex := 10;
    end;
    case fSequence[I] of
    '(' : begin
            UsingParens:=True;
            Inc(I);
          end;
    ')' : begin
            UsingParens:=False;
            PopUpShiftKeys;
            Inc(I);
          end;
    '%' : begin
             fShiftsDown[ShiftIndex+1] := True;
             SendKeyDown(ShiftsVK[ShiftIndex+1] ,1,False);
             if ShiftIndex > 4 then
               Inc(I, 2)
             else
               Inc(I);
          end;
    '+' :  begin
             fShiftsDown[ShiftIndex+2] := True;
             SendKeyDown(ShiftsVK[ShiftIndex+2] ,1,False);
             if ShiftIndex > 4 then
               Inc(I, 2)
             else
               Inc(I);
           end;
    '^' :  begin
             fShiftsDown[ShiftIndex] := True;
             SendKeyDown(ShiftsVK[ShiftIndex] ,1,False);
             if ShiftIndex > 4 then
               Inc(I, 2)
             else
               Inc(I);
           end;
    '#' :  begin
             fShiftsDown[ShiftIndex+3] := True;
             SendKeyDown(ShiftsVK[ShiftIndex+3] ,1,False);
             if ShiftIndex > 4 then
               Inc(I, 2)
             else
               Inc(I);
           end;
    '&' :  begin
             fShiftsDown[ShiftIndex+4] := True;
             SendKeyDown(ShiftsVK[ShiftIndex+4] ,1,False);
             if ShiftIndex > 4 then
               Inc(I, 2)
             else
               Inc(I);
           end;
    '{' : begin
            NumTimes:=1;
            If (fSequence[Succ(I)]='{') then begin
              MKey:=VK_LEFTBRACKET;
              SetBit(Wbytes(MKey)[1],VKKEYSCANSHIFTON);
              SendKey(MKey,1,True);
              If (not UsingParens) then
                PopUpShiftKeys;
              Inc(I,3);
              Continue;
            end;
            KeyString:='';
            FoundClose:=False;
            While (I<=L) do begin
              Inc(I);
              If (fSequence[I]='}') then begin
                FoundClose:=True;
                Inc(I);
                Break;
              end;
              KeyString:=KeyString+Upcase(fSequence[I]);
            end;
            If (Not FoundClose) then begin
               Glb.LogError('No close bracket.', cKeyLogger);
               Exit;
            end;
            If (fSequence[I]='}') then begin
              MKey:=VK_RIGHTBRACKET;
              SetBit(Wbytes(MKey)[1],VKKEYSCANSHIFTON);
              SendKey(MKey,1,True);
              If (not UsingParens) then
                PopUpShiftKeys;
              Inc(I);
              Continue;
            end;
            PosSpace:=Pos(' ',KeyString);
            If (PosSpace<>0) then begin
               NumTimes:=StrToInt(Copy(KeyString,Succ(PosSpace),Length(KeyString)-PosSpace));
               KeyString:=Copy(KeyString,1,Pred(PosSpace));
            end;
            If (Length(KeyString)=1) then MKey:=vkKeyScan(KeyString[1])
            else MKey:=StringToVKey(KeyString);
            If (MKey<>INVALIDKEY) then begin
              SendKey(MKey,NumTimes,True);
              If (not UsingParens) then
                PopUpShiftKeys;
              Continue;
            end;
          end;
    '~' : begin
            SendKeyDown(VK_RETURN,1,True);
            If (not UsingParens) then
              PopUpShiftKeys;
            Inc(I);
          end;
    else  begin
             MKey:=vkKeyScan(fSequence[I]);
             If (MKey<>INVALIDKEY) then begin
               SendKey(MKey,1,True);
               PopUpShiftKeys;
             end else Glb.LogError('Invalid KeyName', cKeyLogger);
             Inc(I);
          end;
    end;
  end;
  PopUpShiftKeys;
end;

function TKeySequence.BitSet(BitTable, BitMask : Byte) : Boolean;
begin
  Result:=ByteBool(BitTable and BitMask);
end;

procedure TKeySequence.SetBit(var BitTable : Byte; BitMask : Byte);
begin
  BitTable:=BitTable or Bitmask;
end;

procedure TKeySequence.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'SKS');
end;

Procedure TKeySequence.KeyboardEvent(VKey, ScanCode : Byte; Flags : Longint);
var
  KeyboardMsg : TMsg;
begin
    keybd_event(VKey, ScanCode, Flags,13);
    //StrPCopy(tmp, 'KEYB: Sending scan code ' + IntToStr(VKey));
    //OutputDebugString(tmp);
    //If (Wait) then While (PeekMessage(KeyboardMsg,0,WM_KEYFIRST, WM_KEYLAST, PM_REMOVE)) do begin
    //  TranslateMessage(KeyboardMsg);
    //  DispatchMessage(KeyboardMsg);
    //end;
end;

Procedure TKeySequence.SendKeyDown(VKey: Byte; NumTimes : Word; GenUpMsg : Boolean);
var
  Cnt : Word;
  ScanCode : Byte;
  NumState : Boolean;
  KeyBoardState : TKeyboardState;
begin
  If (VKey=VK_NUMLOCK) then begin
    NumState:=ByteBool(GetKeyState(VK_NUMLOCK) and 1);
    GetKeyBoardState(KeyBoardState);
    If NumState then KeyBoardState[VK_NUMLOCK]:=(KeyBoardState[VK_NUMLOCK] and not 1)
    else KeyBoardState[VK_NUMLOCK]:=(KeyBoardState[VK_NUMLOCK] or 1);
    SetKeyBoardState(KeyBoardState);
    exit;
  end;

  ScanCode:=Lo(MapVirtualKey(VKey,0));
  For Cnt:=1 to NumTimes do
    If (VKey in ExtendedVKeys)then begin
      KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY);
      If (GenUpMsg) then
      begin
      if fDelayKeys > 0 then
        Sleep(fDelayKeys);
        KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP);
      end
      else
        if fDelayModifiers > 0 then
          Sleep(fDelayModifiers);
    end else begin
      KeyboardEvent(VKey, ScanCode, 0);
      If (GenUpMsg) then
      begin
        if fDelayKeys > 0 then
          Sleep(fDelayKeys);
        KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
      end
      else
        if fDelayModifiers > 0 then
          Sleep(fDelayModifiers);
    end;
end;

Procedure TKeySequence.SendKeyUp(VKey: Byte);
var
  ScanCode : Byte;
begin
  ScanCode:=Lo(MapVirtualKey(VKey,0));
  If (VKey in ExtendedVKeys)then
    KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY and KEYEVENTF_KEYUP)
  else KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
end;

Procedure TKeySequence.SendKey(MKey: Word; NumTimes : Word; GenDownMsg : Boolean);
begin
  If (BitSet(Hi(MKey),VKKEYSCANSHIFTON)) then SendKeyDown(VK_SHIFT,1,False);
  If (BitSet(Hi(MKey),VKKEYSCANCTRLON)) then SendKeyDown(VK_CONTROL,1,False);
  If (BitSet(Hi(MKey),VKKEYSCANALTON)) then SendKeyDown(VK_MENU,1,False);
  SendKeyDown(Lo(MKey), NumTimes, GenDownMsg);
  If (BitSet(Hi(MKey),VKKEYSCANSHIFTON)) then SendKeyUp(VK_SHIFT);
  If (BitSet(Hi(MKey),VKKEYSCANCTRLON)) then SendKeyUp(VK_CONTROL);
  If (BitSet(Hi(MKey),VKKEYSCANALTON)) then SendKeyUp(VK_MENU);
end;

{Implements a simple binary search to locate special key name strings}

Function TKeySequence.StringToVKey(KeyString : ShortString) : Word;
var
  Found, Collided : Boolean;
  Bottom, Top, Middle : Byte;
begin
  Result:=INVALIDKEY;
  Bottom:=1;
  Top:=MaxSendKeyRecs;
  Found:=false;
  Middle:=(Bottom+Top) div 2;
  Repeat
    Collided:=((Bottom=Middle) or (Top=Middle));
    If (KeyString=SendKeyRecs[Middle].Name) then begin
       Found:=True;
       Result:=SendKeyRecs[Middle].VKey;
    end else begin
       If (KeyString>SendKeyRecs[Middle].Name) then Bottom:=Middle
       else Top:=Middle;
       Middle:=(Succ(Bottom+Top)) div 2;
    end;
  Until (Found or Collided);
  If (Result=INVALIDKEY) then Glb.LogError('Invalid Key Name', cKeyLogger);
end;

procedure TKeySequence.PopUpShiftKeys;
var I:Integer;
begin
  for I := Low(fShiftsDown) to High(fShiftsDown) do
  begin
    if fShiftsDown[I] then SendKeyUp(ShiftsVK[I]);
    fShiftsDown[I] := False;
  end
end;


end.
