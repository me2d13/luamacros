unit uRawInput;
 
interface
 
uses Windows;
 
//
// Raw Input Messages.
//
 
type
  HRAWINPUT = THANDLE;
  {$EXTERNALSYM HRAWINPUT}
 
//
// WM_INPUT wParam
//
 
//
// Use this macro to get the input code from wParam.
//
 
function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): DWORD;
{$EXTERNALSYM GET_RAWINPUT_CODE_WPARAM}
 
//
// The input is in the regular message flow,
// the app is required to call DefWindowProc
// so that the system can perform clean ups.
//
 
const
  RIM_INPUT       = 0;
  {$EXTERNALSYM RIM_INPUT}
 
//
// The input is sink only. The app is expected
// to behave nicely.
//
 
  RIM_INPUTSINK   = 1;
  {$EXTERNALSYM RIM_INPUTSINK}
 
//
// Raw Input data header
//
 
type
  tagRAWINPUTHEADER = record
    dwType: DWORD;
    dwSize: DWORD;
    hDevice: THANDLE;
    wParam: WPARAM;
  end;
  {$EXTERNALSYM tagRAWINPUTHEADER}
  RAWINPUTHEADER = tagRAWINPUTHEADER;
  {$EXTERNALSYM RAWINPUTHEADER}
  PRAWINPUTHEADER = ^RAWINPUTHEADER;
  {$EXTERNALSYM PRAWINPUTHEADER}
  LPRAWINPUTHEADER = ^RAWINPUTHEADER;
  {$EXTERNALSYM LPRAWINPUTHEADER}
  TRawInputHeader = RAWINPUTHEADER;
 
//
// Type of the raw input
//
 
const
  RIM_TYPEMOUSE      = 0;
  {$EXTERNALSYM RIM_TYPEMOUSE}
  RIM_TYPEKEYBOARD   = 1;
  {$EXTERNALSYM RIM_TYPEKEYBOARD}
  RIM_TYPEHID        = 2;
  {$EXTERNALSYM RIM_TYPEHID}
 
//
// Raw format of the mouse input
//
 
type
  tagRAWMOUSE = record
    //
    // Indicator flags.
    //
    usFlags: WORD;
 
    //
    // The transition state of the mouse buttons.
    //
 
    union: record
    case Integer of
      0: (
        ulButtons: ULONG);
      1: (
        usButtonFlags: WORD;
        usButtonData: WORD);
    end;
 
    //
    // The raw state of the mouse buttons.
    //
    ulRawButtons: ULONG;
 
    //
    // The signed relative or absolute motion in the X direction.
    //
    lLastX: LongInt;
 
    //
    // The signed relative or absolute motion in the Y direction.
    //
    lLastY: LongInt;
 
    //
    // Device-specific additional information for the event.
    //
    ulExtraInformation: ULONG;
  end;
  {$EXTERNALSYM tagRAWMOUSE}
  RAWMOUSE = tagRAWMOUSE;
  {$EXTERNALSYM RAWMOUSE}
  PRAWMOUSE = ^RAWMOUSE;
  {$EXTERNALSYM PRAWMOUSE}
  LPRAWMOUSE = ^RAWMOUSE;
  {$EXTERNALSYM LPRAWMOUSE}
  TRawMouse = RAWMOUSE;
 
//
// Define the mouse button state indicators.
//
 
const
  RI_MOUSE_LEFT_BUTTON_DOWN   = $0001; // Left Button changed to down.
  {$EXTERNALSYM RI_MOUSE_LEFT_BUTTON_DOWN}
  RI_MOUSE_LEFT_BUTTON_UP     = $0002; // Left Button changed to up.
  {$EXTERNALSYM RI_MOUSE_LEFT_BUTTON_UP}
  RI_MOUSE_RIGHT_BUTTON_DOWN  = $0004; // Right Button changed to down.
  {$EXTERNALSYM RI_MOUSE_RIGHT_BUTTON_DOWN}
  RI_MOUSE_RIGHT_BUTTON_UP    = $0008; // Right Button changed to up.
  {$EXTERNALSYM RI_MOUSE_RIGHT_BUTTON_UP}
  RI_MOUSE_MIDDLE_BUTTON_DOWN = $0010; // Middle Button changed to down.
  {$EXTERNALSYM RI_MOUSE_MIDDLE_BUTTON_DOWN}
  RI_MOUSE_MIDDLE_BUTTON_UP   = $0020; // Middle Button changed to up.
  {$EXTERNALSYM RI_MOUSE_MIDDLE_BUTTON_UP}
 
  RI_MOUSE_BUTTON_1_DOWN = RI_MOUSE_LEFT_BUTTON_DOWN;
  {$EXTERNALSYM RI_MOUSE_BUTTON_1_DOWN}
  RI_MOUSE_BUTTON_1_UP   = RI_MOUSE_LEFT_BUTTON_UP;
  {$EXTERNALSYM RI_MOUSE_BUTTON_1_UP}
  RI_MOUSE_BUTTON_2_DOWN = RI_MOUSE_RIGHT_BUTTON_DOWN;
  {$EXTERNALSYM RI_MOUSE_BUTTON_2_DOWN}
  RI_MOUSE_BUTTON_2_UP   = RI_MOUSE_RIGHT_BUTTON_UP;
  {$EXTERNALSYM RI_MOUSE_BUTTON_2_UP}
  RI_MOUSE_BUTTON_3_DOWN = RI_MOUSE_MIDDLE_BUTTON_DOWN;
  {$EXTERNALSYM RI_MOUSE_BUTTON_3_DOWN}
  RI_MOUSE_BUTTON_3_UP   = RI_MOUSE_MIDDLE_BUTTON_UP;
  {$EXTERNALSYM RI_MOUSE_BUTTON_3_UP}
 
  RI_MOUSE_BUTTON_4_DOWN = $0040;
  {$EXTERNALSYM RI_MOUSE_BUTTON_4_DOWN}
  RI_MOUSE_BUTTON_4_UP   = $0080;
  {$EXTERNALSYM RI_MOUSE_BUTTON_4_UP}
  RI_MOUSE_BUTTON_5_DOWN = $0100;
  {$EXTERNALSYM RI_MOUSE_BUTTON_5_DOWN}
  RI_MOUSE_BUTTON_5_UP   = $0200;
  {$EXTERNALSYM RI_MOUSE_BUTTON_5_UP}
 
//
// If usButtonFlags has RI_MOUSE_WHEEL, the wheel delta is stored in usButtonData.
// Take it as a signed value.
//
 
  RI_MOUSE_WHEEL = $0400;
  {$EXTERNALSYM RI_MOUSE_WHEEL}
 
//
// Define the mouse indicator flags.
//
 
  MOUSE_MOVE_RELATIVE      = 0;
  {$EXTERNALSYM MOUSE_MOVE_RELATIVE}
  MOUSE_MOVE_ABSOLUTE      = 1;
  {$EXTERNALSYM MOUSE_MOVE_ABSOLUTE}
  MOUSE_VIRTUAL_DESKTOP    = $02; // the coordinates are mapped to the virtual desktop
  {$EXTERNALSYM MOUSE_VIRTUAL_DESKTOP}
  MOUSE_ATTRIBUTES_CHANGED = $04; // requery for mouse attributes
  {$EXTERNALSYM MOUSE_ATTRIBUTES_CHANGED}
 
//
// Raw format of the keyboard input
//
 
type
  tagRAWKEYBOARD = record
    //
    // The "make" scan code (key depression).
    //
    MakeCode: WORD;
 
    //
    // The flags field indicates a "break" (key release) and other
    // miscellaneous scan code information defined in ntddkbd.h.
    //
    Flags: WORD;
 
    Reserved: WORD;
 
    //
    // Windows message compatible information
    //
    VKey: WORD;
    Message: UINT;
 
    //
    // Device-specific additional information for the event.
    //
    ExtraInformation: ULONG;
  end;
  {$EXTERNALSYM tagRAWKEYBOARD}
  RAWKEYBOARD = tagRAWKEYBOARD;
  {$EXTERNALSYM RAWKEYBOARD}
  PRAWKEYBOARD = ^RAWKEYBOARD;
  {$EXTERNALSYM PRAWKEYBOARD}
  LPRAWKEYBOARD = ^RAWKEYBOARD;
  {$EXTERNALSYM LPRAWKEYBOARD}
  TRawKeyBoard = RAWKEYBOARD;
 
//
// Define the keyboard overrun MakeCode.
//
 
const
  KEYBOARD_OVERRUN_MAKE_CODE = $FF;
  {$EXTERNALSYM KEYBOARD_OVERRUN_MAKE_CODE}
 
//
// Define the keyboard input data Flags.
//
 
  RI_KEY_MAKE            = 0;
  {$EXTERNALSYM RI_KEY_MAKE}
  RI_KEY_BREAK           = 1;
  {$EXTERNALSYM RI_KEY_BREAK}
  RI_KEY_E0              = 2;
  {$EXTERNALSYM RI_KEY_E0}
  RI_KEY_E1              = 4;
  {$EXTERNALSYM RI_KEY_E1}
  RI_KEY_TERMSRV_SET_LED = 8;
  {$EXTERNALSYM RI_KEY_TERMSRV_SET_LED}
  RI_KEY_TERMSRV_SHADOW  = $10;
  {$EXTERNALSYM RI_KEY_TERMSRV_SHADOW}
 
//
// Raw format of the input from Human Input Devices
//
 
type
  tagRAWHID = record
    dwSizeHid: DWORD;    // byte size of each report
    dwCount: DWORD;      // number of input packed
    bRawData: array [0..0] of BYTE;
  end;
  {$EXTERNALSYM tagRAWHID}
  RAWHID = tagRAWHID;
  {$EXTERNALSYM RAWHID}
  PRAWHID = ^RAWHID;
  {$EXTERNALSYM PRAWHID}
  LPRAWHID = ^RAWHID;
  {$EXTERNALSYM LPRAWHID}
  TRawHid = RAWHID;
 
//
// RAWINPUT data structure.
//
 
  tagRAWINPUT = record
    header: RAWINPUTHEADER;
    case Integer of
      0: (mouse: RAWMOUSE);
      1: (keyboard: RAWKEYBOARD);
      2: (hid: RAWHID);
  end;
  {$EXTERNALSYM tagRAWINPUT}
  RAWINPUT = tagRAWINPUT;
  {$EXTERNALSYM RAWINPUT}
  PRAWINPUT = ^RAWINPUT;
  {$EXTERNALSYM PRAWINPUT}
  LPRAWINPUT = ^RAWINPUT;
  {$EXTERNALSYM LPRAWINPUT}
  TRawInput = RAWINPUT;
 
function RAWINPUT_ALIGN(x: Pointer): Pointer;
{$EXTERNALSYM RAWINPUT_ALIGN}
 
function NEXTRAWINPUTBLOCK(ptr: PRawInput): PRawInput;
{$EXTERNALSYM NEXTRAWINPUTBLOCK}
 
//
// Flags for GetRawInputData
//
 
const
  RID_INPUT  = $10000003;
  {$EXTERNALSYM RID_INPUT}
  RID_HEADER = $10000005;
  {$EXTERNALSYM RID_HEADER}
 
function GetRawInputData(hRawInput: HRAWINPUT; uiCommand: UINT; pData: POINTER;
  var pcbSize: UINT; cbSizeHeader: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputData}
 
//
// Raw Input Device Information
//
 
const
  RIDI_PREPARSEDDATA = $20000005;
  {$EXTERNALSYM RIDI_PREPARSEDDATA}
  RIDI_DEVICENAME    = $20000007; // the return valus is the character length, not the byte size
  {$EXTERNALSYM RIDI_DEVICENAME}
  RIDI_DEVICEINFO    = $2000000b;
  {$EXTERNALSYM RIDI_DEVICEINFO}
 
type
  PRID_DEVICE_INFO_MOUSE = ^RID_DEVICE_INFO_MOUSE;
  {$EXTERNALSYM PRID_DEVICE_INFO_MOUSE}
  tagRID_DEVICE_INFO_MOUSE = record
    dwId: DWORD;
    dwNumberOfButtons: DWORD;
    dwSampleRate: DWORD;
  end;
  {$EXTERNALSYM tagRID_DEVICE_INFO_MOUSE}
  RID_DEVICE_INFO_MOUSE = tagRID_DEVICE_INFO_MOUSE;
  {$EXTERNALSYM RID_DEVICE_INFO_MOUSE}
  TRidDeviceInfoMouse = RID_DEVICE_INFO_MOUSE;
  PRidDeviceInfoMouse = PRID_DEVICE_INFO_MOUSE;
 
  PRID_DEVICE_INFO_KEYBOARD = ^RID_DEVICE_INFO_KEYBOARD;
  {$EXTERNALSYM PRID_DEVICE_INFO_KEYBOARD}
  tagRID_DEVICE_INFO_KEYBOARD = record
    dwType: DWORD;
    dwSubType: DWORD;
    dwKeyboardMode: DWORD;
    dwNumberOfFunctionKeys: DWORD;
    dwNumberOfIndicators: DWORD;
    dwNumberOfKeysTotal: DWORD;
  end;
  {$EXTERNALSYM tagRID_DEVICE_INFO_KEYBOARD}
  RID_DEVICE_INFO_KEYBOARD = tagRID_DEVICE_INFO_KEYBOARD;
  {$EXTERNALSYM RID_DEVICE_INFO_KEYBOARD}
  TRidDeviceInfoKeyboard = RID_DEVICE_INFO_KEYBOARD;
  PRidDeviceInfoKeyboard = PRID_DEVICE_INFO_KEYBOARD;
 
  PRID_DEVICE_INFO_HID = ^RID_DEVICE_INFO_HID;
  {$EXTERNALSYM PRID_DEVICE_INFO_HID}
  tagRID_DEVICE_INFO_HID = record
    dwVendorId: DWORD;
    dwProductId: DWORD;
    dwVersionNumber: DWORD;
    //
    // Top level collection UsagePage and Usage
    //
    usUsagePage: WORD;
    usUsage: WORD;
  end;
  {$EXTERNALSYM tagRID_DEVICE_INFO_HID}
  RID_DEVICE_INFO_HID = tagRID_DEVICE_INFO_HID;
  {$EXTERNALSYM RID_DEVICE_INFO_HID}
  TRidDeviceInfoHid = RID_DEVICE_INFO_HID;
  PRidDeviceInfoHid = PRID_DEVICE_INFO_HID;
 
  tagRID_DEVICE_INFO = record
    cbSize: DWORD;
    dwType: DWORD;
    case Integer of
    0: (mouse: RID_DEVICE_INFO_MOUSE);
    1: (keyboard: RID_DEVICE_INFO_KEYBOARD);
    2: (hid: RID_DEVICE_INFO_HID);
  end;
  {$EXTERNALSYM tagRID_DEVICE_INFO}
  RID_DEVICE_INFO = tagRID_DEVICE_INFO;
  {$EXTERNALSYM RID_DEVICE_INFO}
  PRID_DEVICE_INFO = ^RID_DEVICE_INFO;
  {$EXTERNALSYM PRID_DEVICE_INFO}
  LPRID_DEVICE_INFO = ^RID_DEVICE_INFO;
  {$EXTERNALSYM LPRID_DEVICE_INFO}
  TRidDeviceInfo = RID_DEVICE_INFO;
  PRidDeviceInfo = PRID_DEVICE_INFO;
 
function GetRawInputDeviceInfoA(hDevice: THANDLE; uiCommand: UINT; pData: POINTER;
  var pcbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputDeviceInfoA}
function GetRawInputDeviceInfoW(hDevice: THANDLE; uiCommand: UINT; pData: POINTER;
  var pcbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputDeviceInfoW}
 
{$IFDEF UNICODE}
function GetRawInputDeviceInfo(hDevice: THANDLE; uiCommand: UINT; pData: POINTER;
  var pcbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputDeviceInfo}
{$ELSE}
function GetRawInputDeviceInfo(hDevice: THANDLE; uiCommand: UINT; pData: POINTER;
  var pcbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputDeviceInfo}
{$ENDIF}
 
//
// Raw Input Bulk Read: GetRawInputBuffer
//
 
function GetRawInputBuffer(pData: PRAWINPUT; var pcbSize: UINT; cbSizeHeader: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputBuffer}
 
//
// Raw Input request APIs
//
 
type
  LPRAWINPUTDEVICE = ^RAWINPUTDEVICE;
  {$EXTERNALSYM LPRAWINPUTDEVICE}
  PRAWINPUTDEVICE = ^RAWINPUTDEVICE;
  {$EXTERNALSYM PRAWINPUTDEVICE}
  tagRAWINPUTDEVICE = record
    usUsagePage: WORD; // Toplevel collection UsagePage
    usUsage: WORD;     // Toplevel collection Usage
    dwFlags: DWORD;
    hwndTarget: HWND;    // Target hwnd. NULL = follows keyboard focus
  end;
  {$EXTERNALSYM tagRAWINPUTDEVICE}
  RAWINPUTDEVICE = tagRAWINPUTDEVICE;
  {$EXTERNALSYM RAWINPUTDEVICE}
  TRawInputDevice = RAWINPUTDEVICE;
 
const
  RIDEV_REMOVE       = $00000001;
  {$EXTERNALSYM RIDEV_REMOVE}
  RIDEV_EXCLUDE      = $00000010;
  {$EXTERNALSYM RIDEV_EXCLUDE}
  RIDEV_PAGEONLY     = $00000020;
  {$EXTERNALSYM RIDEV_PAGEONLY}
  RIDEV_NOLEGACY     = $00000030;
  {$EXTERNALSYM RIDEV_NOLEGACY}
  RIDEV_INPUTSINK    = $00000100;
  {$EXTERNALSYM RIDEV_INPUTSINK}
  RIDEV_CAPTUREMOUSE = $00000200; // effective when mouse nolegacy is specified, otherwise it would be an error
  {$EXTERNALSYM RIDEV_CAPTUREMOUSE}
  RIDEV_NOHOTKEYS    = $00000200; // effective for keyboard.
  {$EXTERNALSYM RIDEV_NOHOTKEYS}
  RIDEV_APPKEYS      = $00000400;  // effective for keyboard.
  {$EXTERNALSYM RIDEV_APPKEYS}
  RIDEV_EXMODEMASK   = $000000F0;
  {$EXTERNALSYM RIDEV_EXMODEMASK}
 
function RIDEV_EXMODE(mode: DWORD): DWORD;
{$EXTERNALSYM RIDEV_EXMODE}
 
function RegisterRawInputDevices(pRawInputDevices: PRAWINPUTDEVICE;
  uiNumDevices: UINT; cbSize: UINT): BOOL; stdcall;
{$EXTERNALSYM RegisterRawInputDevices}
 
function GetRegisteredRawInputDevices(pRawInputDevices: PRAWINPUTDEVICE;
  var puiNumDevices: UINT; cbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRegisteredRawInputDevices}
 
type
  PRAWINPUTDEVICELIST = ^RAWINPUTDEVICELIST;
  {$EXTERNALSYM PRAWINPUTDEVICELIST}
  tagRAWINPUTDEVICELIST = record
    hDevice: THANDLE;
    dwType: DWORD;
  end;
  {$EXTERNALSYM tagRAWINPUTDEVICELIST}
  RAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  {$EXTERNALSYM RAWINPUTDEVICELIST}
  TRawInputDeviceList = RAWINPUTDEVICELIST;
 
function GetRawInputDeviceList(pRawInputDeviceList: PRAWINPUTDEVICELIST; var puiNumDevices: UINT;
  cbSize: UINT): UINT; stdcall;
{$EXTERNALSYM GetRawInputDeviceList}
 
//function DefRawInputProc(paRawInput: PRAWINPUT; nInput: Integer; cbSizeHeader: UINT): LRESULT; stdcall;
{ The documentation says that this procedure is called "with the same parameters received by the window procedure"
  so, eventhough the documentation shows the parameter list above, I have changed it to
  the standard windows message procedure parameters.}
function DefRawInputProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
//function DefRawInputProc(Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$EXTERNALSYM DefRawInputProc}
 
implementation
 
const
  user32 = 'user32.dll';
 
function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): DWORD;
begin
  Result := wParam and $ff;
end;
 
function RAWINPUT_ALIGN(x: Pointer): Pointer;
begin
  Result := Pointer((Integer(x) + SizeOf(DWORD) - 1) and not (SizeOf(DWORD) - 1));
end;
 
function NEXTRAWINPUTBLOCK(ptr: PRawInput): PRawInput;
begin
  Result := PRAWINPUT(DWORD(RAWINPUT_ALIGN(ptr)) + ptr^.header.dwSize);
end;
 
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputData: Pointer;
 
function GetRawInputData;
begin
  GetProcedureAddress(_GetRawInputData, user32, 'GetRawInputData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputData]
  end;
end;
{$ELSE}
function GetRawInputData; external user32 name 'GetRawInputData';
{$ENDIF DYNAMIC_LINK}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfoA: Pointer;
 
function GetRawInputDeviceInfoA;
begin
  GetProcedureAddress(_GetRawInputDeviceInfoA, user32, 'GetRawInputDeviceInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfoA]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfoA; external user32 name 'GetRawInputDeviceInfoA';
{$ENDIF DYNAMIC_LINK}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfoW: Pointer;
 
function GetRawInputDeviceInfoW;
begin
  GetProcedureAddress(_GetRawInputDeviceInfoW, user32, 'GetRawInputDeviceInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfoW]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfoW; external user32 name 'GetRawInputDeviceInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfo: Pointer;
 
function GetRawInputDeviceInfo;
begin
  GetProcedureAddress(_GetRawInputDeviceInfo, user32, 'GetRawInputDeviceInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfo]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfo; external user32 name 'GetRawInputDeviceInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfo: Pointer;
 
function GetRawInputDeviceInfo;
begin
  GetProcedureAddress(_GetRawInputDeviceInfo, user32, 'GetRawInputDeviceInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfo]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfo; external user32 name 'GetRawInputDeviceInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputBuffer: Pointer;
 
function GetRawInputBuffer;
begin
  GetProcedureAddress(_GetRawInputBuffer, user32, 'GetRawInputBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputBuffer]
  end;
end;
{$ELSE}
function GetRawInputBuffer; external user32 name 'GetRawInputBuffer';
{$ENDIF DYNAMIC_LINK}
 
function RIDEV_EXMODE(mode: DWORD): DWORD;
begin
  Result := mode and RIDEV_EXMODEMASK;
end;
 
 
{$IFDEF DYNAMIC_LINK}
var
  _RegisterRawInputDevices: Pointer;
 
function RegisterRawInputDevices;
begin
  GetProcedureAddress(_RegisterRawInputDevices, user32, 'RegisterRawInputDevices');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterRawInputDevices]
  end;
end;
{$ELSE}
function RegisterRawInputDevices; external user32 name 'RegisterRawInputDevices';
{$ENDIF DYNAMIC_LINK}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRegisteredRawInputDevices: Pointer;
 
function GetRegisteredRawInputDevices;
begin
  GetProcedureAddress(_GetRegisteredRawInputDevices, user32, 'GetRegisteredRawInputDevices');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRegisteredRawInputDevices]
  end;
end;
{$ELSE}
function GetRegisteredRawInputDevices; external user32 name 'GetRegisteredRawInputDevices';
{$ENDIF DYNAMIC_LINK}
 
{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceList: Pointer;
 
function GetRawInputDeviceList;
begin
  GetProcedureAddress(_GetRawInputDeviceList, user32, 'GetRawInputDeviceList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceList]
  end;
end;
{$ELSE}
function GetRawInputDeviceList; external user32 name 'GetRawInputDeviceList';
{$ENDIF DYNAMIC_LINK}
 
{$IFDEF DYNAMIC_LINK}
var
  _DefRawInputProc: Pointer;
 
function DefRawInputProc;
begin
  GetProcedureAddress(_DefRawInputProc, user32, 'DefRawInputProc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefRawInputProc]
  end;
end;
{$ELSE}
function DefRawInputProc; external user32 name 'DefRawInputProc';
{$ENDIF DYNAMIC_LINK}
 
end.
