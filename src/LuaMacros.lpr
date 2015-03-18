program LuaMacros;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uniqueinstance_package, LazSerialPort, uMainFrm, MemMap, uXplCommon,
  uXplControl, uGlobals, uLuaCmdXpl, uDxDeviceService, uDevice, uDxDevice,
  udeviceservice, uLuaCmdDevice, uLuaEngine, uComDevice, uComDeviceService,
  uKbdDevice, uKbdDeviceService, uRawInput, uHookService, uHookCommon,
  uKeyLogService, uSendKeys;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TLmcMainForm, MainForm);
  Glb.Init;
  MainForm.Init;
  Application.Run;
end.

