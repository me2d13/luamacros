unit uMidiInputDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice;

type
  { TMidiInputDevice }

  TMidiInputDevice = class(TDevice)
  private
    deviceIndex: integer;

  public
    property Index: integer read deviceIndex write deviceIndex;
    function TypeCaption: string; override;

  end;

implementation


function TMidiInputDevice.TypeCaption: string;
begin
  Result := 'midiinput';
end;

end.
