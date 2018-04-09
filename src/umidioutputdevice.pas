unit uMidiOutputDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice;

type
  { TMidiOutputDevice }

  TMidiOutputDevice = class(TDevice)
  private
    deviceIndex: integer;

  public
    property Index: integer read deviceIndex write deviceIndex;
    function TypeCaption: string; override;

  end;

implementation


function TMidiOutputDevice.TypeCaption: string;
begin
  Result := 'midioutput';
end;

end.
