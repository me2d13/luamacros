unit uMainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics,
  Dialogs, PairSplitter, ExtCtrls, ComCtrls, StdCtrls, ActnList, uLuaEngine;

type

  { TMainForm }

  TMainForm = class(TForm)
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
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
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
    procedure SetEditorDirty(AValue: boolean);
    procedure SetFileName(AValue: String);
    procedure BuildFormCaption;
    procedure ProcesApplicationParams;
    procedure LoadFile(const pFileName: String);
    function CheckDirtyTrueCanContinue: boolean;
    function SaveTrueCanContinue: boolean;
    function SaveAsTrueCanContinue: boolean;
  public
    { public declarations }
    procedure print(what: String);
    procedure ClearLog;
    property EditorDirty: boolean read fEditorDirty write SetEditorDirty;
    property FileName: String read fFileName write SetFileName;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  uGlobals;

const
  cUntitled = 'Untitled';

{ TMainForm }

procedure TMainForm.RunScriptActionExecute(Sender: TObject);
var lSel: String;
begin
  lSel := SynEdit1.SelText;
  if (Length(Trim(lSel)) > 0) then
      Glb.LuaEngine.runCode(lSel)
  else
      Glb.LuaEngine.runCode(SynEdit1.Lines.GetText);
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
  SaveTrueCanContinue;
end;

procedure TMainForm.SaveAsActionExecute(Sender: TObject);
begin
  SaveAsTrueCanContinue;
end;

procedure TMainForm.SynEdit1Change(Sender: TObject);
begin
  EditorDirty:=true;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Glb.TickMe;
end;


procedure TMainForm.SetEditorDirty(AValue: boolean);
begin
  if fEditorDirty=AValue then Exit;
  fEditorDirty:=AValue;
  BuildFormCaption;
end;

procedure TMainForm.SetFileName(AValue: String);
begin
  if fFileName=AValue then Exit;
  fFileName:=AValue;
  BuildFormCaption;
end;

procedure TMainForm.BuildFormCaption;
var
  lDirtyFlag: String;
begin
  if fEditorDirty then
    lDirtyFlag:='*'
  else
    lDirtyFlag:='';
  Caption:=Format('LuaMacros - %s%s', [fFileName, lDirtyFlag]);
end;

procedure TMainForm.ProcesApplicationParams;
begin
  if FileExists(Application.Params[1]) then
  begin
    LoadFile(Application.Params[1]);
  end;
end;

procedure TMainForm.LoadFile(const pFileName: String);
begin
  if not FileExists(pFileName) then
    exit;
  SynEdit1.Lines.LoadFromFile(pFileName);
  fEditorDirty:=false;
  FileName:=pFileName;
end;

function TMainForm.CheckDirtyTrueCanContinue: boolean;
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

function TMainForm.SaveTrueCanContinue: boolean;
begin
  if (FileName = cUntitled) then
    Result := SaveAsTrueCanContinue
  else
  begin
    SynEdit1.Lines.SaveToFile(FileName);
    EditorDirty:=false;
  end;
end;

function TMainForm.SaveAsTrueCanContinue: boolean;
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

procedure TMainForm.print(what: String);
begin
  Memo1.Lines.Add(what);
end;

procedure TMainForm.ClearLog;
begin
  Memo1.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Glb.LogFunction:=@print;
  if (Application.ParamCount > 0) then
    ProcesApplicationParams
  else
  begin
    fFileName:=cUntitled;
    EditorDirty:=false;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckDirtyTrueCanContinue;
end;

procedure TMainForm.OpenFileActionExecute(Sender: TObject);
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

