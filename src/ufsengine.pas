unit uFsEngine;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TFsService }

  TFsService = class
  private
    fhSimConnect: THandle;               // Handle for the SimConection
    fSCAvailable : boolean;
    fSCConnected : boolean;
    fRegisteredEvents : TStringList;
    procedure DebugLog(Value: String);
    procedure DebugLogFmt(pFormat:String; pArgs: array of const);
    procedure Connect();
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Init;
    function SendEvent(Name: String; Param: Cardinal = 0): boolean;
  end;

implementation

uses
  uGlobals, SimConnect, windows;

const
  WM_USER_SIMCONNECT = WM_USER + 13;


{ TFsService }

procedure TFsService.DebugLog(Value: String);
begin
  if Glb <> nil then
    Glb.DebugLog(Value, cLoggerFsxx);
end;

procedure TFsService.DebugLogFmt(pFormat: String; pArgs: array of const);
begin
  if Glb <> nil then
    Glb.DebugLogFmt(pFormat, pArgs, cLoggerFsxx);
end;

procedure TFsService.Connect();
begin
  if not fSCConnected then
    begin
      if (SUCCEEDED(SimConnect_Open(fhSimConnect, 'LuaMacros', Glb.MainFormHandle, WM_USER_SIMCONNECT, 0, 0))) then
      begin
        fSCConnected := True;
        fRegisteredEvents.Clear;
      end else begin
        DebugLog('SimConnect connection failure');
      end;
    end else begin
      DebugLog('SimConnect already connected');
    end;
end;

constructor TFsService.Create;
begin
  fRegisteredEvents := TstringList.Create;
  fRegisteredEvents.CaseSensitive := False;
end;

destructor TFsService.Destroy;
begin
  fRegisteredEvents.Free;
  inherited Destroy;
end;

procedure TFsService.Init;
begin
  fhSimConnect := 0;
  fRegisteredEvents.Clear;
  if InitSimConnect then
  begin
    fSCAvailable := True;
    DebugLog('SimConnect dll loaded');
  end
  else
  begin
    DebugLog('SimConnect dll loading failed');
    fSCAvailable := False;
    fSCConnected := False;
  end;
end;

function TFsService.SendEvent(Name: String; Param: Cardinal): boolean;
var
  lEventIndex: Integer;
begin
  if not fSCConnected then Connect;
  if not fSCConnected then
  begin
    Result := False;
    exit;
  end;
  // if not yet registered, register
  Result := True;
  lEventIndex := fRegisteredEvents.IndexOf(Name);
  if lEventIndex = -1 then
  begin
    // we have to register this event first
    lEventIndex := fRegisteredEvents.Count;
    if (SUCCEEDED(SimConnect_MapClientEventToSimEvent(fhSimConnect, lEventIndex, Name))) then
    begin
      DebugLog('SC: Event ' + Name + ' registered as ' + IntToStr(lEventINdex));
      fRegisteredEvents.Add(Name);
    end
    else
    begin
      DebugLog('SC: Error registering event ' + Name + ' as ' + IntToStr(lEventIndex));
      Result := False;
      lEventIndex := -1;
    end;
  end;
  // call with its index
  if Result then
  begin
    Result := SUCCEEDED(SimConnect_TransmitClientEvent(fhSimConnect, 0, lEventIndex,
      Param, SIMCONNECT_GROUP_PRIORITY_HIGHEST, SIMCONNECT_EVENT_FLAG_GROUPID_IS_PRIORITY));
  end;
end;
end.

