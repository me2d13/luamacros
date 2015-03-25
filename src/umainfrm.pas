unit uMainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, ComCtrls, StdCtrls, ActnList, SynHighlighterLua,
  UniqueInstance, uLuaEngine, Windows;

const
  WM_LUA_RUN_CHANGE = WM_USER + 310;

type

  { TLmcMainForm }

  TLmcMainForm = class(TForm)
    OpenDialog1: TOpenDialog;
    OpenFileAction: TAction;
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
    UniqueInstance1: TUniqueInstance;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure RunScriptActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fEditorDirty: boolean;
    fFileName: String;
    fPrintCs: TRTLCriticalSection;
    procedure SetEditorDirty(AValue: boolean);
    procedure SetFileName(AValue: String);
    procedure BuildFormCaption;
    procedure ProcesApplicationParams;
    procedure LoadFile(const pFileName: String);
    function CheckDirtyTrueCanContinue: boolean;
    function SaveTrueCanContinue: boolean;
    function SaveAsTrueCanContinue: boolean;
    procedure OrderRawInputMessagesToBeReceived;
  public
    { public declarations }
    procedure print(what: String);
    procedure ClearLog;
    procedure Init;
    // message listeners
    procedure WmInputMessage(var Message: TMessage);
    procedure WmLuaRunChange(var Message: TMessage);

    property EditorDirty: boolean read fEditorDirty write SetEditorDirty;
    property FileName: String read fFileName write SetFileName;
  end;

var
  MainForm: TLmcMainForm;

implementation

{$R *.lfm}

uses
  uGlobals, uHookCommon;

const
  cUntitled = 'Untitled';

var
  PrevWndProc: WNDPROC;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  lMessage : TMessage;
begin
  lMessage.lParam:=lParam;
  lMessage.wParam:=wParam;
  lMessage.msg:=uMsg;
  if uMsg=WM_INPUT then
  begin
    MainForm.WmInputMessage(lMessage);
    Result := lMessage.Result;
    //result:=CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
  end else if uMsg=WM_ASKLMCFORM then
  begin
    Glb.HookService.OnHookMessage(lMessage);
    Result := lMessage.Result;
  end else if uMsg=WM_LUA_RUN_CHANGE then
  begin
    MainForm.WmLuaRunChange(lMessage);
    Result := lMessage.Result;
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
    Glb.LuaEngine.Reset;
    Glb.DeviceService.DetectDevices; // clears device table
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

procedure TLmcMainForm.SynEdit1Change(Sender: TObject);
begin
  EditorDirty:=true;
end;

procedure TLmcMainForm.Timer1Timer(Sender: TObject);
begin
  Glb.TickMe;
end;

procedure TLmcMainForm.WmInputMessage(var Message: TMessage);
begin
  Glb.DeviceService.KbdDeviceService.OnRawMessage(Message);
end;

procedure TLmcMainForm.WmLuaRunChange(var Message: TMessage);
var
  lCaption: String;
begin
  if (Glb.LuaEngine.IsRunning) then
  begin
    lCaption:=Format('Running 1 script, %d queued', [Glb.LuaEngine.GetExecutionQueueSize]);
    RunScriptAction.Enabled:=False;
  end else begin
    RunScriptAction.Enabled:=True;
    lCaption:='Not running';
  end;
  // refresh lua running indicators
  StatusBar1.Panels.Items[0].Text:=lCaption;
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
begin
  if FileExists(Application.Params[1]) then
  begin
    LoadFile(Application.Params[1]);
  end;
end;

procedure TLmcMainForm.LoadFile(const pFileName: String);
begin
  if not FileExists(pFileName) then
    exit;
  SynEdit1.Lines.LoadFromFile(pFileName);
  fEditorDirty:=false;
  FileName:=pFileName;
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

procedure TLmcMainForm.print(what: String);
begin
  EnterCriticalSection(fPrintCs);
  try
    Memo1.Lines.Add(what);
  finally
    LeaveCriticalSection(fPrintCs);
  end;
end;

procedure TLmcMainForm.ClearLog;
begin
  Memo1.Clear;
end;

procedure TLmcMainForm.Init;
begin
  // here Glb is alreadu created & initialized
  OrderRawInputMessagesToBeReceived;
  Glb.HookService.Init(Handle);
  // maybe resume LUA execution thread here to have panel update
end;

procedure TLmcMainForm.FormCreate(Sender: TObject);
begin
  InitCriticalSection(fPrintCs);
  Glb.LogFunction:=@print;
  if (Application.ParamCount > 0) then
    ProcesApplicationParams
  else
  begin
    fFileName:=cUntitled;
    EditorDirty:=false;
  end;
  PrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback)));
end;

procedure TLmcMainForm.FormDestroy(Sender: TObject);
begin
  DoneCriticalsection(fPrintCs);
end;

procedure TLmcMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckDirtyTrueCanContinue;
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

