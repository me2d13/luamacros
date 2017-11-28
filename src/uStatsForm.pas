unit uStatsForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TStatisticsForm }

  TStatisticsForm = class(TForm)
    CopyButton: TButton;
    SummaryMemo: TMemo;
    PageControl1: TPageControl;
    DevicesMemo: TMemo;
    SummaryTabSheet: TTabSheet;
    DevicesTabSheet: TTabSheet;
    Timer1: TTimer;
    procedure CopyButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fRefreshCount: Int64;
    procedure Render;
    procedure RenderSummary;
    procedure RenderDevices;
  public
    { public declarations }
  end;

var
  StatisticsForm: TStatisticsForm;

implementation

uses
  uGlobals, uStatsService, uConfigService, Clipbrd, uDevice;

{$R *.lfm}

{ TStatisticsForm }

procedure TStatisticsForm.Timer1Timer(Sender: TObject);
begin
  Inc(fRefreshCount);
  Render;
end;

procedure TStatisticsForm.Render;
begin
  RenderSummary;
  RenderDevices;
end;

procedure TStatisticsForm.RenderSummary;
begin
  with SummaryMemo.Lines do
  begin
    BeginUpdate;
    Clear;
    Add('Current time: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now)]);
    Add('Refresh count of this window: %d', [fRefreshCount]);
    EndUpdate;
  end;
end;

procedure TStatisticsForm.RenderDevices;
var
  lDevice: TDevice;
  lI: Integer;
begin
  with DevicesMemo.Lines do
  begin
    BeginUpdate;
    Clear;
    lI := 1;
    for lDevice in Glb.DeviceService.Devices do
    begin
      Add(' == Device number %d ==', [lI]);
      Add('  Name         : %s', [lDevice.Name]);
      Add('  System id    : %s', [lDevice.SystemId]);
      Add('  System handle: %d', [lDevice.Handle]);
      Add('  Type         : %s', [lDevice.TypeCaption]);
      Add('');
      Inc(lI);
    end;
    EndUpdate;
  end;
end;

procedure TStatisticsForm.FormCreate(Sender: TObject);
begin
  fRefreshCount:=0;
end;

procedure TStatisticsForm.CopyButtonClick(Sender: TObject);
var
  lBuffer: String;
begin
  lBuffer := '=== Summary ===' + #13;
  lBuffer := lBuffer + SummaryMemo.Lines.Text;
  lBuffer := lBuffer + '=== Devices ===' + #13;
  lBuffer := lBuffer + DevicesMemo.Lines.Text;
  Clipboard.Clear;
  Clipboard.AsText := lBuffer;
end;

procedure TStatisticsForm.FormHide(Sender: TObject);
begin
  Timer1.Enabled:=False;
  Glb.ConfigService.SetBoolean(cParamStats, False);
end;

procedure TStatisticsForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled:=True;
end;

end.

