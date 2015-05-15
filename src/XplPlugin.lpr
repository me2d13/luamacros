library XplPlugin;

{$mode objfpc}{$H+}

uses
    SysUtils,
    Classes,
    Windows,
    //Graphics,
    XPLMDefs, XPLMDisplay, XPLMProcessing, XPLMUtilities, XPLMDataAccess,
    MemMap, uXplPluginEngine, uXplCommon, uXplSender,
    uXplAbstractReceiver, uXplPluginReceiver, uxplmessages;

  // The Delphi Compiler must create a file with a .XPL extention. The default
  // extention for a Delphi library is .DLL - thus we use the $E compiler
  // directive to override the default and change it to .XPL
  {$E xpl}

  // Default directive to include any RESOURCE files our project may have

  var
   //gLatitutde : XPLMDataRef;
   gEng: TXplEngine;


  function MyFlightLoopCallbackFunc(inElapsedSinceLastCall    : Single;
                                inElapsedTimeSinceLastFlightLoop : Single;
                                inCounter : LongInt;
                                inRefcon   : pointer ) : Single; cdecl;
var
 lBuff: PChar;
 lMessage: String;
  begin
    //* If any data refs are missing, do not proceed.
    if (gEng = nil) then
    begin
      result := 1;
      exit;
    end;

  try
    gEng.XplTick;
  except
    on E: Exception do
    begin
      lMessage:=Format('LUAMACROS exception: %s', [e.Message]);
      GetMem(lBuff, Length(lMessage) + 1);
      try
        StrPCopy(lBuff, lMessage);
        XPLMDebugString(lBuff);
      finally
        FreeMem(lBuff);
      end;
    end;
  end;

    result := -1;
  end;


function MyDrawCallbackFunc(inPhase             : XPLMDrawingPhase;
                                    inIsBefore          : integer;
                                    inRefcon            : pointer) : Integer; cdecl;
  var
   lBuff: PChar;
   lMessage: String;
  begin
    //* If any data refs are missing, do not proceed.
    if (gEng = nil)  then
    begin
      result := 1;
      exit;
    end;

    try
      gEng.DrawText();
    except
      on E: Exception do
      begin
        lMessage:=Format('LUAMACROS exception: %s', [e.Message]);
        GetMem(lBuff, Length(lMessage) + 1);
        try
          StrPCopy(lBuff, lMessage);
          XPLMDebugString(lBuff);
        finally
          FreeMem(lBuff);
        end;
      end;
    end;

    result := -1;
  end;

  {/*
   * XPluginStart
   *
   * Our start routine registers our callback and does any other initialization we
   * must do.
   *
   */}
  function XPluginStart( outName : PChar;
                         outSig  : PChar;
                         outDesc : PChar ) : integer; cdecl;
  begin

    //* First record our plugin information.
    StrPCopy(outName, 'LuamacrosController');
    StrPCopy( outSig,  'PetrMedek.LuaMacros.Controller' );
    StrPCopy(outDesc, 'A plugin to access sim variables in LuaMacros.');

    gEng := TXplEngine.Create;

    //outputPath := @Buffer;
  	//XPLMGetSystemPath(outputPath);

  	{/* Find the data refs we want to record. */}
  	//gLatitutde := XPLMFindDataRef('sim/flightmodel/position/latitude');

    if gEng <> nil then
    begin
      XPLMRegisterFlightLoopCallback(@MyFlightLoopCallbackFunc, -1, nil);
      XPLMRegisterDrawCallback(@MyDrawCallbackFunc, xplm_Phase_Window, 1, nil);
    end;

    result := 1
  end;

  {/*
   * XPluginStop
   *
   * Our cleanup routine unregistering our callback.
   *
   */}
  procedure XPluginStop; cdecl;
  begin
  	{/* Unregister the callback */}
    XPLMUnregisterDrawCallback(@MyDrawCallbackFunc, xplm_Phase_Window, 1, nil);
  	XPLMUnregisterFlightLoopCallback(@MyFlightLoopCallbackFunc, nil);
    gEng.Free;
    gEng := nil;
  end;

  {/*
   * XPluginDisable
   *
   * We do not need to do anything when we are disabled, but we must provide the handler.
   *
   */}
  procedure XPluginDisable; cdecl;
  begin
    // nothing to do
  end;

  {/*
   * XPluginEnable.
   *
   * We don't do any enable-specific initialization, but we must return 1 to indicate
   * that we may be enabled at this time.
   *
   */}
  function XPluginEnable : integer; cdecl;
  begin
    result := 1;
  end;

  {/*
   * XPluginReceiveMessage
   *
   * We don't have to do anything in our receive message handler, but we must provide one.
   *
   */}
  procedure XPluginReceiveMessage(   inFromWho : XPLMPluginID;
                                     inMessage : longint;
                                     inParam   : pointer); cdecl;
  begin
    // nothing to do
  end;

  // In order to make these calls in our DLL visible to the outside world, we
  // need to export them.
  exports
    XPluginStop,
    XPluginStart,
    XPluginReceiveMessage,
    XPluginEnable,
    XPluginDisable;

  begin
  end.

  //eof


