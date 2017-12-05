unit uStatsService;

{$mode delphi}

interface

uses
  Classes, fgl, SysUtils;

type

  { TLuaCallInfo }

  TLuaCallInfo = class
  private
    fStartsCount: Integer;
    fEndsCount: Integer;
    fTotalDuration: Int64;
  public
    property StartsCount: Integer read fStartsCount;
    property EndsCount: Integer read fEndsCount;
    property TotalDuration: Int64 read fTotalDuration;
    procedure AddStart;
    procedure AddEnd(pDuration: Int64);
  end;

  { TXplVarInfo }

  TXplVarInfo = class
  private
    fReadCount: Integer;
    fWriteCount: Integer;
  public
    property ReadCount: Integer read fReadCount;
    property WriteCount: Integer read fWriteCount;
    procedure AddRead;
    procedure AddWrite;
  end;

  TLuaCallInfoMap = TFPGMap<String, TLuaCallInfo>;
  TXplVarInfoMap = TFPGMap<String, TXplVarInfo>;

  { TStatsService }

  TStatsService = class
    private
      fLuaCalls: TLuaCallInfoMap;
      fXplCalls: TStringList;
      fXplVarInfoMap: TXplVarInfoMap;
      function FindOrCreate(pName: String): TLuaCallInfo;
      function FindOrCreateXpl(pName: String): TXplVarInfo;
    public
      constructor Create;
      destructor Destroy; virtual;
      procedure Reset;
      function BeginCommand(pName: String): Int64;
      procedure EndCommand(pName: String; pStartTs: Int64);
      procedure ReadXplVar(pName: String);
      procedure WriteXplVar(pName: String);
      procedure XplCommandExecuted(pName: String);
      property LuaCalls: TLuaCallInfoMap read fLuaCalls;
      property XplCommands: TStringList read fXplCalls;
      property XplVarInfoMap: TXplVarInfoMap read fXplVarInfoMap;
  end;

implementation

uses uGlobals;

{ TXplVarInfo }

procedure TXplVarInfo.AddRead;
begin
  Inc(fReadCount);
end;

procedure TXplVarInfo.AddWrite;
begin
  Inc(fWriteCount);
end;

{ TLuaCallInfo }

procedure TLuaCallInfo.AddStart;
begin
  Inc(fStartsCount);
end;

procedure TLuaCallInfo.AddEnd(pDuration: Int64);
begin
  Inc(fEndsCount);
  Inc(fTotalDuration, pDuration);
end;

{ TStatsService }

function TStatsService.FindOrCreate(pName: String): TLuaCallInfo;
var
  lI: Integer;
begin
  lI := fLuaCalls.IndexOf(pName);
  if lI < 0 then
  begin
    Result := TLuaCallInfo.Create();
    fLuaCalls.Add(pName, Result);
  end else begin
    Result := fLuaCalls.Data[lI];
  end;
end;

function TStatsService.FindOrCreateXpl(pName: String): TXplVarInfo;
var
  lI: Integer;
begin
  lI := fXplVarInfoMap.IndexOf(pName);
  if lI < 0 then
  begin
    Result := TXplVarInfo.Create();
    fXplVarInfoMap.Add(pName, Result);
  end else begin
    Result := fXplVarInfoMap.Data[lI];
  end;
end;

constructor TStatsService.Create;
begin
  fLuaCalls := TLuaCallInfoMap.Create;
  fXplCalls := TStringList.Create;
  fXplVarInfoMap := TXplVarInfoMap.Create;
end;

destructor TStatsService.Destroy;
var
  lI: Integer;
begin
  for lI := 0 to fLuaCalls.Count - 1 do
    fLuaCalls.Data[lI].Free;
  fLuaCalls.Free;
  fXplCalls.Free;
  for lI := 0 to fXplVarInfoMap.Count - 1 do
    fXplVarInfoMap.Data[lI].Free;
  fXplVarInfoMap.Free;
  inherited;
end;

procedure TStatsService.Reset;
var
  lI: Integer;
begin
  for lI := 0 to fLuaCalls.Count - 1 do
    fLuaCalls.Data[lI].Free;
  fLuaCalls.Clear;
  fXplCalls.Clear;
  for lI := 0 to fXplVarInfoMap.Count - 1 do
    fXplVarInfoMap.Data[lI].Free;
  fXplVarInfoMap.Clear;
end;

function TStatsService.BeginCommand(pName: String): Int64;
begin
  FindOrCreate(pName).AddStart;
  Result := Glb.UnixTimestampMs;
end;

procedure TStatsService.EndCommand(pName: String; pStartTs: Int64);
begin
  FindOrCreate(pName).AddEnd(Glb.UnixTimestampMs - pStartTs);
end;

procedure TStatsService.ReadXplVar(pName: String);
begin
  FindOrCreateXpl(pName).AddRead;
end;

procedure TStatsService.WriteXplVar(pName: String);
begin
  FindOrCreateXpl(pName).AddWrite;
end;

procedure TStatsService.XplCommandExecuted(pName: String);
var
  lI : Integer;
  lVal: Integer;
begin
  lI := fXplCalls.IndexOfName(pName);
  if (lI < 0) then
    fXplCalls.Values[pName] := '1'
  else
  begin
    lVal := StrToInt(fXplCalls.ValueFromIndex[lI]);
    fXplCalls.Values[pName] := IntToStr(lVal+1);
  end;
end;

end.

