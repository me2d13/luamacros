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
    LuaMemo: TMemo;
    SummaryMemo: TMemo;
    PageControl1: TPageControl;
    DevicesMemo: TMemo;
    SummaryTabSheet: TTabSheet;
    DevicesTabSheet: TTabSheet;
    LuaTabSheet: TTabSheet;
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
    procedure RenderLua;
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
  RenderLua;
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

procedure TStatisticsForm.RenderLua;
var
  lI: Integer;
begin
  with LuaMemo.Lines do
  begin
    BeginUpdate;
    Clear;
    Add('Lua script executions count (without callbacks) : %d', [Glb.LuaEngine.ExecutionsCount]);
    Add('Lua script executions time (without callbacks) : %d ms', [Glb.LuaEngine.ExecutionsTime]);
    Add('');
    Add('Builtin command executions (total runs, failed runs, total time [ms], avg time per exec [ms]:');
    for lI := 0 to Glb.StatsService.LuaCalls.Count -1 do
    begin
      Add(Format('  %20s: %9d %5d %10d %9.3f', [
        Glb.StatsService.LuaCalls.Keys[lI],
        Glb.StatsService.LuaCalls.Data[lI].StartsCount,
        Glb.StatsService.LuaCalls.Data[lI].StartsCount - Glb.StatsService.LuaCalls.Data[lI].EndsCount,
        Glb.StatsService.LuaCalls.Data[lI].TotalDuration,
        Glb.StatsService.LuaCalls.Data[lI].TotalDuration / Glb.StatsService.LuaCalls.Data[lI].EndsCount
      ]));
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
  lBuffer := lBuffer + '=== Lua ===' + #13;
  lBuffer := lBuffer + LuaMemo.Lines.Text;
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

