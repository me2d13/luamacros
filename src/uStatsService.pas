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

  TLuaCallInfoMap = TFPGMap<String, TLuaCallInfo>;

  { TStatsService }

  TStatsService = class
    private
      fLuaCalls: TLuaCallInfoMap;
      function FindOrCreate(pName: String): TLuaCallInfo;
    public
      constructor Create;
      destructor Destroy; virtual;
      function BeginCommand(pName: String): Int64;
      procedure EndCommand(pName: String; pStartTs: Int64);
      property LuaCalls: TLuaCallInfoMap read fLuaCalls;
  end;

implementation

uses uGlobals;

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
  if not fLuaCalls.Find(pName, lI) then
  begin
    Result := TLuaCallInfo.Create();
    fLuaCalls.KeyData[pName] := Result;
  end else begin
    Result := fLuaCalls.Data[lI];
  end;
end;

constructor TStatsService.Create;
begin
  fLuaCalls := TLuaCallInfoMap.Create();
end;

destructor TStatsService.Destroy;
var
  lI: Integer;
begin
  for lI := 0 to fLuaCalls.Count - 1 do
    fLuaCalls.Data[lI].Free;
  fLuaCalls.Free;
  inherited;
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

end.

