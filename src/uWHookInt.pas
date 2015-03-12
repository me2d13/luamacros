unit uWHookInt;

interface

uses
  Windows, Messages, SysUtils, uHookCommon;

function SetHook: Boolean; stdcall; export;
function FreeHook: Boolean; stdcall; export;
function MsgFilterFuncKbd(Code: longint; wParam: WPARAM; lParam: LPARAM): LRESULT stdcall; export;


implementation

uses MemMap;

const
  WH_KEYBOARD_LL = 13;

type
 { Structure used by WH_KEYBOARD_LL }
 PKBDLLHookStruct = ^TKBDLLHookStruct;
 {$EXTERNALSYM tagKBDLLHOOKSTRUCT}
 tagKBDLLHOOKSTRUCT = packed record
   vkCode: DWORD;
   scanCode: DWORD;
   flags: DWORD;
   time: DWORD;
   dwExtraInfo: PULONG;
 end;
 TKBDLLHookStruct = tagKBDLLHOOKSTRUCT;
 {$EXTERNALSYM KBDLLHOOKSTRUCT}
 KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;

 ULONG_PTR = ^DWORD;

// Actual hook stuff

type
  TPMsg = ^TMsg;

const
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_M = $4D;
  VK_R = $52;

  // global variables, only valid in the process which installs the hook.
var
  gMemMap: TMemMap;
  gSharedPtr: PMMFData;
  gPid: DWORD;
  gHookKbd: HHOOK;

{
  The SetWindowsHookEx function installs an application-defined
  hook procedure into a hook chain.

  WH_GETMESSAGE Installs a hook procedure that monitors messages
  posted to a message queue.
  For more information, see the GetMsgProc hook procedure.
}

procedure DebugLog(Value: String);
var
  lFile: TextFile;
begin
  if (gSharedPtr <> nil) and (gSharedPtr^.Debug > 0) then
  begin
    AssignFile(lFile, 'LmcDll.log');
    if FileExists('LmcDll.log') then
      Append(lFile)
    else
      Rewrite(lFile);
    Write(lFile, Format('%s [DLL]: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now), Value]));
    WriteLn(lFile);
    CloseFile(lFile);
  end;
end;

function GetPathFromPID(const PID: cardinal): string;
var
  hProcess: THandle;
  path: array[0..MAX_PATH - 1] of char;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, PID);
  if hProcess <> 0 then
    try
      if GetModuleFileName(hProcess, path, MAX_PATH) = 0 then
        RaiseLastOSError;
      result := path;
    finally
      CloseHandle(hProcess)
    end
  else
    RaiseLastOSError;
end;

function SetHook: Boolean; stdcall;
begin
  Result := False;
  if gSharedPtr = nil then
    exit;

  gHookKbd := SetWindowsHookEx(WH_KEYBOARD, @MsgFilterFuncKbd, HInstance, 0);
  if (gHookKbd = 0) then
    FreeHook  // free is something was not ok
  else
    Result := True;
  DebugLog(Format('Hook set for process %d', [gPid]));
end;


{
  The UnhookWindowsHookEx function removes the hook procedure installed
  in a hook chain by the SetWindowsHookEx function.
}

function FreeHook: Boolean; stdcall;
var b1: Boolean;
begin
  Result := False;
  b1 := True;
  if (gHookKbd <> 0) then
    b1 := UnHookWindowsHookEx(gHookKbd);
  Result := b1;
end;



(*
    GetMsgProc(
    nCode: Integer;  {the hook code}
    wParam: WPARAM;  {message removal flag}
    lParam: LPARAM  {a pointer to a TMsg structure}
    ): LRESULT;  {this function should always return zero}

    { See help on ==> GetMsgProc}
*)

function MsgFilterFuncKbd(Code: longint; wParam: WPARAM; lParam: LPARAM): LRESULT stdcall;
var
  Kill: boolean;
  what2do : Integer;
  MessageId: Word;
//  ext_code: Cardinal;
begin
  if (Code < 0) or (Code <> HC_ACTION) or (gSharedPtr = nil) then
  begin
    Result := CallNextHookEx(gHookKbd {ignored in API}, Code, wParam, lParam);
    exit;
  end;
  Result := 0;
  Kill := False;
  if (lParam and $80000000 > 0) then
    MessageId := WM_KEYUP
  else
    MessageId := WM_KEYDOWN;
  what2do := SendMessage(gSharedPtr^.MainWinHandle, WM_ASKLMCFORM, MessageId , wParam);
  if (what2do = -1) then
    Kill := True;
  if Kill then
    Result := 1
  else
    Result := CallNextHookEx(gHookKbd {ignored in API}, Code, wParam, lParam);
end;


initialization
begin
  gPid := GetCurrentProcessId;
  gSharedPtr := nil;
  //DebugLog('Attached to PID ' + IntToStr(gPid));
  try
    gMemMap := TMemMap.Create(MMFName, SizeOf(TMMFData));
    gSharedPtr := gMemMap.Memory;
  except
    on EMemMapException do
    begin
      DebugLog(IntToStr(gPid)+': Can''t create MMF.');
      gMemMap := nil;
    end;
  end;
end;

finalization
  FreeHook;
  if gMemMap <> nil then
    try
      gMemMap.Free;
    except
      on EMemMapException do
        DebugLog(IntToStr(gPid)+': Can''t release MMF.');
    end;
end.
