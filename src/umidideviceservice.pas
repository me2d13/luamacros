unit uMidiDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, Midi, fgl, uMidiInputDevice, Lua;

type
  TMidiInputDeviceList = TFPGObjectList<TMidiInputDevice>;

  { TMidiDeviceService }

  TMidiEventHandlerInfo = class
  public
    Device: TMidiInputDevice;
    LuaRef: integer;
  end;

  TMidiEventHandlerInfoList = TFPGObjectList<TMidiEventHandlerInfo>;

  TMidiDeviceService = class
  private
    fDevices: TMidiInputDeviceList;
    fMidiEventHandlers: TMidiEventHandlerInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function DetectDevices: integer;
    procedure HandleMidiInData(const aDeviceIndex: integer;
      const aStatus, aData1, aData2: byte);
    function GetDeviceByIndex(const aDeviceIndex: integer): TMidiInputDevice;
    procedure SetMidiHandler(pDeviceName: PAnsiChar; pHandlerRef: integer);
  end;

function LuaCmdSetMidiHandler(luaState: TLuaState): integer;

implementation

uses
  uGlobals, uDevice;

{ TMidiDeviceService }

constructor TMidiDeviceService.Create();
begin
  fDevices := TMidiInputDeviceList.Create();
  fMidiEventHandlers := TMidiEventHandlerInfoList.Create();
end;

destructor TMidiDeviceService.Destroy;
var
  device: TMidiInputDevice;

begin
  for device in fDevices do
  begin
    MidiInput.Close(device.Index);

  end;
  fDevices.Free;
end;

function TMidiDeviceService.GetDeviceByIndex(const aDeviceIndex: integer):
TMidiInputDevice;
var
  device: TMidiInputDevice;
begin
  for device in fDevices do
  begin
    if (device.Index = aDeviceIndex) then
    begin
      Result := device;
    end;
  end;
end;

procedure TMidiDeviceService.HandleMidiInData(const aDeviceIndex: integer;
  const aStatus, aData1, aData2: byte);
var
  lMidiEventHandlerInfo: TMidiEventHandlerInfo;
  pDevice: TMidiInputDevice;
begin
  pDevice := GetDeviceByIndex(aDeviceIndex);
  for lMidiEventHandlerInfo in fMidiEventHandlers do
  begin
    if (pDevice = lMidiEventHandlerInfo.Device) then
    begin
      Glb.DebugLogFmt(
        'Device %s midi event triggerred lua handler status %d, data1 %d, data2 %d',
        [pDevice.Name, aStatus, aData1, aData2], cLoggerDx);
      Glb.LuaEngine.CallFunctionByRef(lMidiEventHandlerInfo.LuaRef,
        aStatus, aData1, aData2);
    end;

  end;
end;

procedure TMidiDeviceService.Init();
begin
  MidiInput.OnMidiData := HandleMidiInData;
end;

function TMidiDeviceService.DetectDevices: integer;
var
  I: integer;
  deviceCount: integer;
  midiDevice: string;
  midiIn: TMidiInputDevice;
begin
  deviceCount := MidiInput.Devices.Count;
  for I := 0 to deviceCount - 1 do
  begin
    midiDevice := MidiInput.Devices[I];
    midiIn := TMidiInputDevice.Create;
    midiIn.SystemId := midiDevice;
    midiIn.Index := I;
    MidiInput.Open(I);
    fDevices.Add(midiIn);
    Glb.DeviceService.Devices.Add(midiIn);
  end;

  Result := fDevices.Count;
end;

procedure TMidiDeviceService.SetMidiHandler(pDeviceName: PAnsiChar;
  pHandlerRef: integer);
var
  newHandler: TMidiEventHandlerInfo;
begin
  newHandler := TMidiEventHandlerInfo.Create();
  newHandler.Device := TMidiInputDevice(Glb.DeviceService.GetByName(pDeviceName));
  newHandler.LuaRef := pHandlerRef;
  fMidiEventHandlers.Add(newHandler);
end;


function LuaCmdSetMidiHandler(luaState: TLuaState): integer;
var
  lDeviceName: PAnsiChar;
  lHandlerRef: integer;
  lNumOfParams: integer;
  lStart: int64;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_set_midi_handler');
  // Device name
  // handler
  lNumOfParams := lua_gettop(luaState);
  lDeviceName := lua_tostring(luaState, 1);
  if (lNumOfParams = 2) then
  begin
    lHandlerRef := luaL_ref(luaState, LUA_REGISTRYINDEX);
    Glb.DeviceService.MidiDeviceService.SetMidiHandler(lDeviceName, lHandlerRef);
  end
  else
    raise LmcException.Create('2 parameters expected: device, handler');
  Result := 0;
  Glb.StatsService.EndCommand('lmc_set_midi_handler', lStart);
end;


end.
