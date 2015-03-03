program LuaMacros;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LazSerialPort, uMainFrm, MemMap, uXplCommon, uXplControl, uGlobals,
  uLuaCmdXpl, uDxDeviceService, uDevice, uDxDevice, udeviceservice,
  uLuaCmdDevice, uLuaEngine, uComDevice, uComDeviceService
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Glb.Init;
  Application.Run;
end.

