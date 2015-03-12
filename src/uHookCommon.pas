unit uHookCommon;

interface

uses Messages, Windows;

const
  MMFName = 'LuaMacrosSharedMem';
  WM_ASKLMCFORM = WM_USER + 300;

type
  PMMFData = ^TMMFData;
  TMMFData = record
    MainWinHandle: HWND;
    LmcPID: DWORD;
    Debug: DWORD;  // 0 = no debug
  end;


implementation

end.
