library WinHook;

{$mode objfpc}{$H+}

uses
  Classes, uHookCommon, uWHookInt
  { you can add units after this };

exports
  SetHook index 1,
  FreeHook index 2;
end.

