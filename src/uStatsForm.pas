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
    XplMemo: TMemo;
    SummaryMemo: TMemo;
    PageControl1: TPageControl;
    DevicesMemo: TMemo;
    SummaryTabSheet: TTabSheet;
    DevicesTabSheet: TTabSheet;
    LuaTabSheet: TTabSheet;
    XplTabSheet: TTabSheet;
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
    procedure RenderXpl;
  public
    { public declarations }
  end;

var
  StatisticsForm: TStatisticsForm;

implementation

uses
  uGlobals, uStatsService, uConfigService, Clipbrd, uDevice, uLuaEngine, uXplCommon;

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
  RenderXpl;
end;

procedure TStatisticsForm.RenderSummary;
begin
  with SummaryMemo.Lines do
  begin
    BeginUpdate;
    Clear;
    Add('Current time: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now)]);
    Add('Current Unix ts [ms]: %d', [UnixTimeStampCommonForXpl]);
    Add('Refresh count of this window: %d', [fRefreshCount]);
    EndUpdate;
  end;
end;

procedure TStatisticsForm.RenderDevices;
var
  lDevice: TDevice;
  lCallback: TTrigger;
  lI: Integer;
  lSL: TStringList;
begin
  lSL := TStringList.Create;
  with lSL do
  begin
    Clear;
    lI := 1;
    for lDevice in Glb.DeviceService.Devices do
    begin
      Add(' == Device number %d ==', [lI]);
      Add('  Name         : %s', [lDevice.Name]);
      Add('  System id    : %s', [lDevice.SystemId]);
      Add('  System handle: %d', [lDevice.Handle]);
      Add('  Type         : %s', [lDevice.TypeCaption]);
      for lCallback in Glb.LuaEngine.Callbacks do
      begin
        if lCallback.Device = lDevice then
        begin
          if lCallback.WholeDevice then
            Add('    Whole device callback, lua ref %d ', [lCallback.LuaRef])
          else
            Add('    Signle callback, lua ref %d, key %d, direction %s',
                [lCallback.LuaRef, lCallback.KeyNumber, DirectionToString(lCallback.Direction)])
        end;
      end;
      Add('');
      Inc(lI);
    end;
  end;
  if (lSL.Text <> DevicesMemo.Lines.Text) then
    DevicesMemo.Lines.Text:= lSL.Text;
  lSL.Free;
end;

procedure TStatisticsForm.RenderLua;
var
  lI: Integer;
  lSL: TStringList;
  lAvg: Double;
begin
  lSL := TStringList.Create;
  with lSL do
  begin
    Add('Lua script executions count (without callbacks) : %d', [Glb.LuaEngine.ExecutionsCount]);
    Add('Lua script executions time (without callbacks)  : %d ms', [Glb.LuaEngine.ExecutionsTime]);
    Add('Number of active timers                         : %d', [Glb.TimerService.GetTimersCount]);
    Add('');
    Add('Builtin command executions (total runs, failed runs, total time [ms], avg time per exec [ms]:');
    for lI := 0 to Glb.StatsService.LuaCalls.Count -1 do
    begin
      if (Glb.StatsService.LuaCalls.Data[lI].EndsCount = 0) then
        lAvg := 0
      else
        lAvg := Glb.StatsService.LuaCalls.Data[lI].TotalDuration / Glb.StatsService.LuaCalls.Data[lI].EndsCount;
      Add(Format('  %25s: %9d %5d %10d %9.3f', [
        Glb.StatsService.LuaCalls.Keys[lI],
        Glb.StatsService.LuaCalls.Data[lI].StartsCount,
        Glb.StatsService.LuaCalls.Data[lI].StartsCount - Glb.StatsService.LuaCalls.Data[lI].EndsCount,
        Glb.StatsService.LuaCalls.Data[lI].TotalDuration,
        lAvg
      ]));
    end;
  end;
  if (lSL.Text <> LuaMemo.Lines.Text) then
    LuaMemo.Lines.Text:= lSL.Text;
  lSL.Free;
end;

procedure TStatisticsForm.RenderXpl;
var
  lI: Integer;
  lSL: TStringList;
  lCallback: TLmcVariableCallbackInfo;
begin
  lSL := TStringList.Create;
  with lSL do
  begin
    Add('Executed commands:');
    for lI := 0 to Glb.StatsService.XplCommands.Count - 1 do
    begin
      Add('  %60s : %9d', [Glb.StatsService.XplCommands.Names[lI], StrToInt(Glb.StatsService.XplCommands.ValueFromIndex[lI])]);
    end;
    Add('');
    Add('Variables summary (name, reads, writes):');
    for lI := 0 to Glb.StatsService.XplVarInfoMap.Count - 1 do
    begin
      Add('  %60s : %7d  %7d', [Glb.StatsService.XplVarInfoMap.Keys[lI],
          Glb.StatsService.XplVarInfoMap.Data[lI].ReadCount, Glb.StatsService.XplVarInfoMap.Data[lI].WriteCount]);
    end;
    Add('');
    Add('Variables callbacks (name, activation count):');
    for lI := 0 to Glb.XplControl.Callbacks.Count - 1 do
    begin
      lCallback := Glb.XplControl.Callbacks.Items[lI];
      Add('  %60s : %7d', [lCallback.Name, lCallback.ActivationCount]);
    end;
  end;
  if (lSL.Text <> XplMemo.Lines.Text) then
    XplMemo.Lines.Text:= lSL.Text;
  lSL.Free;
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
  lBuffer := lBuffer + '=== Xplane ===' + #13;
  lBuffer := lBuffer + XplMemo.Lines.Text;
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

