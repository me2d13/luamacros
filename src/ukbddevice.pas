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

  TKeyStrokePtr = ^TKeyStroke;
  TKeyStroke = record
    Device: TKbdDevice;
    DeviceHandle: Integer;
    VKeyCode: Integer;
    Direction: Integer;
  end;


implementation

{ TKbdDevice }

function TKbdDevice.TypeCaption: String;
begin
  Result := 'keyboard';
end;

end.

