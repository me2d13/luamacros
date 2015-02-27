unit uDevice;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TDevice }

  TDevice = class
    private
      fName: String;
      fSystemId: String;
    protected
      function LogIdent: String;
    public
      property Name:String read fName write fName;
      property SystemId: String read fSystemId write fSystemId;
      function TypeCaption: String; virtual;
  end;

const
  cDeviceLoggerName = 'DEV';
  cDirectionDown = 0;
  cDirectionUp = 1;


implementation

uses
  uGlobals;

{ TDevice }

function TDevice.LogIdent: String;
begin
  if (fName = '') then
    Result := Format('%s %s [%s]', [cUnassigned, TypeCaption, fSystemId])
  else
    Result := fName;
end;

function TDevice.TypeCaption: String;
begin
  Result := 'common';
end;

end.

