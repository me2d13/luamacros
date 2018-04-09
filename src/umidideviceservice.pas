unit uMidiDeviceService;

{$mode delphi}

interface

uses
  Classes, SysUtils, Midi, fgl, uMidiInputDevice, uMidiOutputDevice, Lua;

type
  TMidiInputDeviceList = TFPGObjectList<TMidiInputDevice>;
  TMidiOutputDeviceList = TFPGObjectList<TMidiOutputDevice>;
  { TMidiDeviceService }

  TMidiEventHandlerInfo = class
  public
    Device: TMidiInputDevice;
    LuaRef: integer;
  end;

  TMidiEventHandlerInfoList = TFPGObjectList<TMidiEventHandlerInfo>;

  TMidiDeviceService = class
  private
    fInputDevices: TMidiInputDeviceList;
    fOutputDevices: TMidiOutputDeviceList;
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
procedure LuaCmdSendMidiNote(luaState: TLuaState);

implementation

uses
  uGlobals, uDevice;

{ TMidiDeviceService }

constructor TMidiDeviceService.Create();
begin
  fInputDevices := TMidiInputDeviceList.Create();
  fOutputDevices := TMidiOutputDeviceList.Create();
  fMidiEventHandlers := TMidiEventHandlerInfoList.Create();
end;

destructor TMidiDeviceService.Destroy;
var
  device: TMidiInputDevice;
  odevice: TMidiOutputDevice;
begin
  for device in fInputDevices do
  begin
    MidiInput.Close(device.Index);
  end;
  for odevice in fOutputDevices do
  begin
    MidiOutput.Close(device.Index);
  end;
  fInputDevices.Free;
  fOutputDevices.Free;
end;

function TMidiDeviceService.GetDeviceByIndex(
  const aDeviceIndex: integer): TMidiInputDevice;
var
  device: TMidiInputDevice;
begin
  for device in fInputDevices do
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
  midiOut: TMidiOutputDevice;
begin
  deviceCount := MidiInput.Devices.Count;
  for I := 0 to deviceCount - 1 do
  begin
    midiDevice := MidiInput.Devices[I];
    midiIn := TMidiInputDevice.Create;
    midiIn.SystemId := midiDevice + ' IN';
    midiIn.Index := I;
    MidiInput.Open(I);
    fInputDevices.Add(midiIn);
    Glb.DeviceService.Devices.Add(midiIn);
  end;
  deviceCount := MidiOutput.Devices.Count;
  for I := 0 to deviceCount - 1 do
  begin
    midiDevice := MidiOutput.Devices[I];
    midiOut := TMidiOutputDevice.Create;
    midiOut.SystemId := midiDevice + ' OUT';
    midiOut.Index := I;
    MidiOutput.Open(I);
    fOutputDevices.Add(midiOut);
    Glb.DeviceService.Devices.Add(midiOut);
  end;


  Result := fInputDevices.Count + fOutputDevices.Count;
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

procedure LuaCmdSendMidiNote(luaState: TLuaState);
var
  lDeviceName: PAnsiChar;
  lHandlerRef: integer;
  lNumOfParams: integer;
  midiDevice: TMidiOutputDevice;
  lStart: int64;
  aStatus, aData1, aData2: byte;
begin
  lStart := Glb.StatsService.BeginCommand('lmc_send_midi_note');

  lNumOfParams := lua_gettop(luaState);

  if (lNumOfParams = 4) then
  begin
    lDeviceName := lua_tostring(luaState, 1);
    aStatus := lua_tointeger(luaState, 2);
    aData1 := lua_tointeger(luaState, 3);
    aData2 := lua_tointeger(luaState, 4);
    midiDevice := TMidiOutputDevice(Glb.DeviceService.GetByName(lDeviceName));
    // Check for null device
    MidiOutput.Send(midiDevice.Index, aStatus, aData1, aData2);
  end
  else
    raise LmcException.Create('4 parameters expected: device, status, data1, data2');

  Glb.StatsService.EndCommand('lmc_send_midi_note', lStart);
end;

end.
