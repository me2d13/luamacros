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
  uKeyLogService, uSendKeys, uScanService, uLuaCmdMainWindow, uXplListener,
  uxplsender, uXplMessages, uConfigService, uHttpServer, uLuaCmdHttp;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TLmcMainForm, gMainForm);
  Glb.Init;
  gMainForm.Init;
  Application.Run;
end.

