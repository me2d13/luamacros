unit uMainFrm;

{$mode objfpc}{$H+}

{
Command line parameter - script with DX logger on:
  C:\work\luamacros\src\test.lua -L DX
  c:\work\luamacros\src\lmc.lua
}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, ComCtrls, StdCtrls, ActnList, Menus,
  SynHighlighterLua, UniqueInstance, uLuaEngine, Windows;

const
  WM_LUA_RUN_CHANGE = WM_USER + 310;
  WM_LUA_QUEUE_CHANGE = WM_USER + 311;

type

  { TLmcMainForm }

  TLmcMainForm = class(TForm)
    ResetAction: TAction;
    MenuItem2: TMenuItem;
    ShowAction: TAction;
    ExitAction: TAction;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    ScanCancelButton: TButton;
    OpenDialog1: TOpenDialog;
    OpenFileAction: TAction;
    ScanPanel: TPanel;
    SaveAsAction: TAction;
    SaveAction: TAction;
    ImageList1: TImageList;
    RunScriptAction: TAction;
    ActionList1: TActionList;
    Memo1: TMemo;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    SynLuaSyn1: TSynLuaSyn;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    TrayIcon1: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure ResetActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure RunScriptActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure ScanCancelButtonClick(Sender: TObject);
    procedure ShowActionExecute(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ScanningChange;
    procedure ManageTrayIcon;
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { private declarations }
    fEditorDirty: boolean;
    fFileName: String;
    fAutoExecute: Boolean;
    fHookKeyboard: Boolean;
    fLastWriteTime: FILETIME;
    fResetFlag: Boolean;
    procedure SetEditorDirty(AValue: boolean);
    procedure SetFileName(AValue: String);
    procedure BuildFormCaption;
    procedure ProcesApplicationParams;
    procedure LoadFile(const pFileName: String);
    function CheckDirtyTrueCanContinue: boolean;
    function SaveTrueCanContinue: boolean;
    function SaveAsTrueCanContinue: boolean;
    procedure OrderRawInputMessagesToBeReceived;
    procedure StoreFileModificationTime;
    procedure AppGetsFocus(Sender: TObject);
  public
    { public declarations }
    procedure print(what: String);
    procedure ClearLog;
    procedure Init;
    procedure ResetAfterLuaScriptEnd;
    // message listeners
    procedure WmInputMessage(var Message: TMessage);
    procedure WmLuaRunChange(var Message: TMessage);
    function WmMainWindowCommand(wParam: WParam; lParam: LParam):LRESULT;

    property EditorDirty: boolean read fEditorDirty write SetEditorDirty;
    property FileName: String read fFileName write SetFileName;
  end;

var
  gMainForm: TLmcMainForm;
  gMainFormThreadId: DWORD;

const
  cGuiLoggerName = 'GUI';


implementation

{$R *.lfm}

uses
  uGlobals, uHookCommon, uConfigService, comobj, uStatsForm, uUsb;

const
  cUntitled = 'Untitled';

var
  PrevWndProc: WNDPROC;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  lMessage : TMessage;
  Dbi: PDevBroadcastDeviceInterface;
  lName: String;
begin
  lMessage.lParam:=lParam;
  lMessage.wParam:=wParam;
  lMessage.msg:=uMsg;
  if uMsg=WM_INPUT then
  begin
    gMainForm.WmInputMessage(lMessage);
    Result := lMessage.Result;
    //result:=CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
  end else if uMsg=WM_ASKLMCFORM then
  begin
    Glb.HookService.OnHookMessage(lMessage);
    Result := lMessage.Result;
  end else if (uMsg=WM_LUA_RUN_CHANGE) or (uMsg=WM_LUA_QUEUE_CHANGE) then
  begin
    gMainForm.WmLuaRunChange(lMessage);
    Result := lMessage.Result;
  end else if uMsg=WM_FLUSH_PRINT_BUFFER then
  begin
    Glb.FlushBuffer;
    Result := lMessage.Result;
  end else if uMsg=WM_MAIN_WINDOW_COMMAND then
  begin
    Result := gMainForm.WmMainWindowCommand(wParam, lParam);
  end else if uMsg=WM_SCANNING_STATUS_CHANGE then
  begin
    gMainForm.ScanningChange;
    Result := lMessage.Result;
  end else if uMsg=WM_DEVICECHANGE then
  begin
    if (wParam = $8000) or (wParam = $8004) then
    begin
      Dbi := PDevBroadcastDeviceInterface(LParam);
      if Dbi^.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE then
      begin
        lName := PChar(@Dbi^.dbcc_name);
        Glb.Print('HOLA ' + lName);
      end;
    end
  end else
    result:=CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

{ TLmcMainForm }

procedure TLmcMainForm.RunScriptActionExecute(Sender: TObject);
var lSel: String;
begin
  lSel := SynEdit1.SelText;
  if (Length(Trim(lSel)) > 0) then
      Glb.LuaEngine.runCode(lSel)
  else
  begin
    Glb.Reset;
    Glb.LuaEngine.runCode(SynEdit1.Lines.GetText);
  end;
end;

procedure TLmcMainForm.SaveActionExecute(Sender: TObject);
begin
  SaveTrueCanContinue;
end;

procedure TLmcMainForm.SaveAsActionExecute(Sender: TObject);
begin
  SaveAsTrueCanContinue;
end;

procedure TLmcMainForm.ScanCancelButtonClick(Sender: TObject);
begin
  ScanPanel.Visible:=False;
  Glb.ScanService.ScannedDevice := nil;
  Glb.ScanService.ScanEvent.SetEvent;
  Glb.DebugLog('Keyboard scan cancelled.', cGuiLoggerName);
end;

procedure TLmcMainForm.ShowActionExecute(Sender: TObject);
begin
  Show;
end;

procedure TLmcMainForm.SynEdit1Change(Sender: TObject);
begin
  EditorDirty:=true;
end;

procedure TLmcMainForm.Timer1Timer(Sender: TObject);
begin
  Glb.TickMe;
end;

procedure TLmcMainForm.ScanningChange;
begin
  if (Glb.ScanService.Scanning) then
  begin
    ScanPanel.Align := alClient;
    ScanPanel.Caption:=Format('Press a key to identify device %s...', [Glb.ScanService.ScannedName]);
  end;
  ScanPanel.Visible:=Glb.ScanService.Scanning;
end;

procedure TLmcMainForm.ManageTrayIcon;
begin
  if (Glb.ConfigService.GetBoolean(cParamMinimizeToTray)) then
    TrayIcon1.Show
  else
    TrayIcon1.Hide;
end;

procedure TLmcMainForm.TrayIcon1DblClick(Sender: TObject);
begin
  ShowInTaskBar:=stDefault;
  Show;
end;

procedure TLmcMainForm.WmInputMessage(var Message: TMessage);
begin
  Glb.DeviceService.KbdDeviceService.OnRawMessage(Message);
end;

procedure TLmcMainForm.WmLuaRunChange(var Message: TMessage);
var
  lCaption: String;
  lArFlag: String;
  lStatsFlag: Boolean;
begin
  lCaption:=Format('Callback queue %d/%d', [Glb.LuaEngine.GetQueueSize, cMaxQueueSize]);
  if (Glb.LuaEngine.IsRunning) then
  begin
    lCaption:=lCaption + ' (running)';
    RunScriptAction.Enabled:=False;
  end else begin
    RunScriptAction.Enabled:=True;
    ManageTrayIcon;
    if (fResetFlag) then
    begin
      fResetFlag:=False;
      ResetActionExecute(Self);
    end;
  end;
  if (Glb.ConfigService.GetBoolean(cParamAutoReload)) then
    lArFlag:='Auto reload'
  else
    lArFlag:='';
  // refresh lua running indicators
  StatusBar1.Panels.Items[0].Text:=lCaption;
  StatusBar1.Panels.Items[2].Text:=lArFlag;
  lStatsFlag := Glb.ConfigService.GetBoolean(cParamStats);
  if (lStatsFlag) and (not StatisticsForm.Visible) then
    StatisticsForm.Show;
  if (not lStatsFlag) and (StatisticsForm.Visible) then
    StatisticsForm.Hide;
end;

function TLmcMainForm.WmMainWindowCommand(wParam: WParam; lParam: LParam
  ): LRESULT;
var
  lText: String;
begin
  if (wParam = MWC_MINIMIZE) then
  begin
    WindowState:=wsMinimized;
    FormWindowStateChange(self);
  end;
  if (wParam = MWC_LOAD) then
  begin
    LoadFile(Glb.LuaEngine.ScriptToRun);
    Glb.LuaEngine.ScriptToRun := '';
    RunScriptActionExecute(nil);
  end;
end;


procedure TLmcMainForm.SetEditorDirty(AValue: boolean);
begin
  if fEditorDirty=AValue then Exit;
  fEditorDirty:=AValue;
  BuildFormCaption;
end;

procedure TLmcMainForm.SetFileName(AValue: String);
begin
  if fFileName=AValue then Exit;
  fFileName:=AValue;
  BuildFormCaption;
end;

procedure TLmcMainForm.BuildFormCaption;
var
  lDirtyFlag: String;
begin
  if fEditorDirty then
    lDirtyFlag:='*'
  else
    lDirtyFlag:='';
  Caption:=Format('LuaMacros - %s%s', [fFileName, lDirtyFlag]);
end;

procedure TLmcMainForm.ProcesApplicationParams;
var
  i: Integer;
begin
  i := 1;
  while i <= Application.ParamCount do
  begin
    if (UpperCase(Application.Params[i]) = '-R') then
    begin
      fAutoExecute := True; // start after complete init is done
    end else if (UpperCase(Application.Params[i]) = '-K') then
    begin
      // do not hook keyboards globaly which can cause weird keyboard behavior (mainly for dead keys)
      // used when lms should run in the background and you don't need (use) keyboard macros
      fHookKeyboard := False;
    end else if (UpperCase(Application.Params[i]) = '-L') and (i < Application.ParamCount) then
    begin
      Inc(i);
      Glb.LogModule(Application.Params[i]);
    end else if FileExists(Application.Params[i]) then
    begin
      LoadFile(Application.Params[i]);
    end else begin
      Glb.LogError('Unexpected parameter (or file not readable): ' + Application.Params[i], cGuiLoggerName);
    end;
    Inc(i);
  end;
end;

procedure TLmcMainForm.LoadFile(const pFileName: String);
begin
  if not FileExists(pFileName) then
    exit;
  SynEdit1.Lines.LoadFromFile(pFileName);
  EditorDirty:=false;
  FileName:=pFileName;
  StoreFileModificationTime;
end;

function TLmcMainForm.CheckDirtyTrueCanContinue: boolean;
var
  lResult: TModalResult;
begin
  Result := True;
  if EditorDirty and (FileName <> cUntitled) and (SynEdit1.Lines.Count > 0) then
  begin
    lResult := MessageDlg('Current file is not saved. Save changes?', mtWarning, mbYesNoCancel, 0);
    if (lResult = mrCancel) then
      Result := false
    else if (lResult = mrNo) then
      Result := true
    else
    begin
      Result := SaveTrueCanContinue;
    end;
  end;
end;

function TLmcMainForm.SaveTrueCanContinue: boolean;
begin
  if (FileName = cUntitled) then
    Result := SaveAsTrueCanContinue
  else
  begin
    SynEdit1.Lines.SaveToFile(FileName);
    StoreFileModificationTime;
    EditorDirty:=false;
  end;
end;

function TLmcMainForm.SaveAsTrueCanContinue: boolean;
begin
  if (FileName <> cUntitled) then
    SaveDialog1.FileName:=FileName;
  if SaveDialog1.Execute then
  begin
    FileName:=SaveDialog1.FileName;
    SynEdit1.Lines.SaveToFile(FileName);
    StoreFileModificationTime;
    EditorDirty:=false;
    Result := True;
  end
  else
    Result := False;
end;

procedure TLmcMainForm.OrderRawInputMessagesToBeReceived;
begin
  Glb.DeviceService.KbdDeviceService.OrderRawInputMessagesToBeReceived(Handle);
end;

procedure TLmcMainForm.StoreFileModificationTime;
var
  fad: TWin32FileAttributeData;
begin
  if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @fad) then
    RaiseLastOSError;
  fLastWriteTime := fad.ftLastWriteTime;
end;

procedure TLmcMainForm.AppGetsFocus(Sender: TObject);
var
  fad: TWin32FileAttributeData;
begin
  if (FileName <> cUntitled) then
  begin
    if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @fad) then
      RaiseLastOSError;
    if (fLastWriteTime.dwHighDateTime <> fad.ftLastWriteTime.dwHighDateTime) or
      (fLastWriteTime.dwLowDateTime <> fad.ftLastWriteTime.dwLowDateTime) then
    begin
      if (Glb.ConfigService.GetBoolean(cParamAutoReload)) then
      begin
        LoadFile(FileName);
      end else begin
        EditorDirty:=true;
        StoreFileModificationTime;
      end;
    end;
  end;
end;

procedure TLmcMainForm.print(what: String);
begin
  Memo1.Lines.Add(what);
end;

procedure TLmcMainForm.ClearLog;
begin
  Memo1.Clear;
end;

procedure TLmcMainForm.Init;
var
  lUsbNotifier : TUsbNotifier;
begin
  lUsbNotifier := TUsbNotifier.Create;
  // here Glb is alreadu created & initialized
  OrderRawInputMessagesToBeReceived;
  if (fHookKeyboard) then
  begin
    Glb.HookService.Init(Handle);
  end;
  if (fAutoExecute) then
  begin
    RunScriptAction.Execute;
  end;
  // maybe resume LUA execution thread here to have panel update
end;

procedure TLmcMainForm.ResetAfterLuaScriptEnd;
begin
  fResetFlag:=True;
end;

procedure TLmcMainForm.FormCreate(Sender: TObject);
begin
  gMainFormThreadId := GetCurrentThreadID;
  fAutoExecute := False;
  fResetFlag:=False;
  fHookKeyboard := True;
  Glb.LogFunction:=@print;
  Glb.MainFormHandle:=handle;
  if (Application.ParamCount > 0) then
    ProcesApplicationParams
  else
  begin
    fFileName:=cUntitled;
    EditorDirty:=false;
  end;
  PrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback)));
  Glb.Version:=Sto_GetFmtFileVersion();
  StatusBar1.Panels.Items[1].Text:= 'Version: ' + Glb.Version;
  Timer1.Interval:=Glb.ConfigService.GetInteger(cParamDxTimerIntervalMs);
  Application.OnActivate:=@AppGetsFocus;
end;

procedure TLmcMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TLmcMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) and (Glb.ConfigService.GetBoolean(cParamMinimizeToTray)) then
  begin
    WindowState:=wsNormal;
    Hide;
    ShowInTaskBar:=stNever;
  end;
end;

procedure TLmcMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckDirtyTrueCanContinue;
end;

procedure TLmcMainForm.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TLmcMainForm.ResetActionExecute(Sender: TObject);
begin
  Glb.Reset;
end;

procedure TLmcMainForm.OpenFileActionExecute(Sender: TObject);
begin
  if (EditorDirty) then
  begin
    if not CheckDirtyTrueCanContinue then
      exit;
  end;
  if OpenDialog1.Execute then
  begin
    LoadFile(OpenDialog1.FileName);
  end;
end;

end.

