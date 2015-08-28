{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazSerialPort;

interface

uses
  LazSerial, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazSerial', @LazSerial.Register);
end;

initialization
  RegisterPackage('LazSerialPort', @Register);
end.
