unit uKbdDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils, uDevice;


type

  { TKbdDevice }

  TKbdDevice = class(TDevice)
    private

    public
      function TypeCaption: String; override;

  end;

const
  cKbdLoggerName = 'KBD';


implementation

{ TKbdDevice }

function TKbdDevice.TypeCaption: String;
begin
  Result := 'keyboard';
end;

end.

