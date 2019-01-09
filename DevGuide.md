# Intro
This is detailed development guide with exact steps how to build luamacros.exe. Following this guide you will have running build environment
and will be able to create pull request for fixes & improvements in luamacros. It's intended for people with pascal knowledge and programming
skills. You don't need this guide if you're luamacros user writing your lua scripts.

# IDE setup
* Download & install Lazarus environment: http://www.lazarus-ide.org/. At the time this guide is written (Jan 2019) I'll use the latest version
which is 1.8.4 for windows 64bit. After installation during first run Lazarus may complain about missing fpc (free pascal) sources. It my they case
sources were already installed with Lazarus just the path was wrong. So I fixed the path explicitely to Lazarus home - fpc - 3.0.4 - source
* clone Luamacros code from this repository: https://github.com/me2d13/luamacros.git
* if you try to open the project now it will claim abut missing packages (you may try)
* so let's install the packages first. From Lazarus 1.8 there's Online Package manager (OPM - http://wiki.freepascal.org/Online_Package_Manager) so
  you don't need to install packages manually. But first you need to manually install OPM itself. Go to menu Package - Install/Uninstall packages and
  from Available for installation select onlinepackagemanager. Click Save and rebuild IDE and Continue
* Now OPM is available, go to menu Package - Online Package Manager. Let's install dependencies one by one
* For lua syntax highlight in editor we need extra highlighters. In search section of OPM type 'extra' and select both extrahighlighters.lpk and 
  extrahighlighters_dsgn.lpk. Click install and then Yes to rebuild IDE.
* In similar way install package uniqueinstance_package.lpk
* In similar way install package LazSerialPort.lpk
* In similar way install package lnetbase.lpk

# Included projects
LuaMacros application consists of 3 Lazarus projects
* LuaMacros.lpi which is the main application - the window you see if you start LuaMacros.exe. This is 32bit windows application for now
* XplPlugin.lpi is project that generates Xplane plugin win.xpl. This is basically windows dll library (just renamed to .xpl) but it's 64bit as Xplane itself is 64bit now. This makes compilation of this project little more complicated but you don't need to compile xpl plugin if you're not using Xplane and/or do not plan to change the communication between plugin and application
* WinHook.lpi produces WinHook.dll file which is needed to block original key when keyboard macro is executed. This is related to real "internals" of LuaMacros and you may not need to touch it. However there is no challenge to compile this one - is uses the same settings as main (exe) project

# FPC compilers
As you downloaded 64bit version of Lazarus it uses by default 64bit free pascal (fpc) compiler. This is located in Lazarus_install_dir\fpc\fpc_version\bin\x86_64-win64. However to compile LuaMacros.exe as 32bit application you need also FPC 32bit compiler. Download one at https://www.freepascal.org/down/i386/win32.var (see version note below), install it to some location and configure Lazarus to use this fpc.exe to compile project luamacros.

LuaMacros can be now compiled with FPC compiler version 3.0.0. Using the latest 3.0.4 throws some compilation errors which I need to fix (Jan 2019). Until those fixes are in place you have to use FPC version 3.0.0.

# Building your luamacros.exe
Now you should be able to open project LuaMacros.lpi and compile to create LuaMacros.exe. If you try to run this file, don't forget to copy
lua dlls to out directory (to the same dir where LuaMacros.exe is created). The lua dlls are lua52-64.dll and lua52-32.dll available in luamacros download package and also in lib folder in github repo.

To compile xpl plugin (project XplPlugin.lpi) you need to make sure that 64bit version of FPC compiler is used. 