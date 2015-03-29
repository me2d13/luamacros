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
      fHandle: THandle;
    protected
      function LogIdent: String;
    public
      property Name:String read fName write fName;
      property SystemId: String read fSystemId write fSystemId;
      property Handle: THandle read fHandle write fHandle;
      function TypeCaption: String; virtual;
  end;

  TDeviceException = class (Exception);

const
  cDeviceLoggerName = 'DEV';
  cDirectionDown = 1;
  cDirectionUp = 0;


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

