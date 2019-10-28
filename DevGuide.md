# Intro
This is detailed development guide with exact steps on how to build luamacros.exe. Following this guide you will have a running build environment
and will be able to create pull requests for fixes & improvements in LuaMacros. It's intended for people with Pascal knowledge and programming
skills. You don't need this guide if you're a LuaMacros user writing your Lua scripts.

# IDE setup
* Download & install the Lazarus environment: http://www.lazarus-ide.org/. At the time this guide is written (Jan 2019) I'll use the latest version
which is 1.8.4 for Windows 64 bit. After installation, during the first run, Lazarus may complain about missing fpc (free Pascal) sources. It my case
the sources were already installed with Lazarus; just the path was wrong. So I fixed the path explicitely to Lazarus home - fpc - 3.0.4 - source
* Clone Luamacros code from this repository: https://github.com/me2d13/luamacros.git
* If you try to open the project now it will complain abut missing packages (you may try)
* So let's install the packages first. From Lazarus 1.8 there's the Online Package manager (OPM - http://wiki.freepascal.org/Online_Package_Manager) so
  you don't need to install packages manually. But first you need to manually install OPM itself. Go to menu Package - Install/Uninstall packages and
  from Available for installation, select onlinepackagemanager. Click Save and rebuild IDE and Continue
* Now OPM is available. Go to menu Package - Online Package Manager. Let's install dependencies one by one
* For Lua syntax highlight in the editor we need extra highlighters. In the search section of OPM, type 'extra' and select both extrahighlighters.lpk and 
  extrahighlighters_dsgn.lpk. Click install and then Yes to rebuild the IDE.
* In a similar way, install package uniqueinstance_package.lpk
* In a similar way, install package LazSerialPort.lpk
* In a similar way, install package lnetbase.lpk

# Included projects
The LuaMacros application consists of three Lazarus projects
* LuaMacros.lpi which is the main application - the window you see if you start LuaMacros.exe. This is a 32-bit Windows application for now.
* XplPlugin.lpi is a project that generates the X-Plane plugin win.xpl. This is basically a Windows DLL file library (just renamed to .xpl), but it's 64 bit as X-Plane itself is 64 bit now. This makes compilation of this project a little more complicated, but you don't need to compile the xpl plugin if you're not using X-Plane and/or do not plan to change the communication between the plugin and the application.
* WinHook.lpi produces the WinHook.dll file which is needed to block the original key when a keyboard macro is executed. This is related to the real "internals" of LuaMacros, and you may not need to touch it. However, there is no challenge to compile this one - is uses the same settings as the main (EXE file) project.

# FPC compilers
As you downloaded the 64-bit version of Lazarus it uses by default the 64-bit free Pascal (fpc) compiler. This is located in Lazarus_install_dir\fpc\fpc_version\bin\x86_64-win64. However, to compile LuaMacros.exe as a 32-bit application, you need also the FPC 32-bit compiler. Download one at https://www.freepascal.org/down/i386/win32.var (see version note below), install it to some location, and configure Lazarus to use this fpc.exe to compile project LuaMacros.

LuaMacros can be now compiled with the FPC compiler version 3.0.0. Using the latest 3.0.4 throws some compilation errors which I need to fix (Jan 2019). Until those fixes are in place, you have to use FPC version 3.0.0.

# Building your luamacros.exe
Now you should be able to open the project LuaMacros.lpi and compile to create LuaMacros.exe. If you try to run this file, don't forget to copy
Lua DLL files to the out directory (to the same directory where LuaMacros.exe is created). The Lua DLL files are lua52-64.dll and lua52-32.dll available in the LuaMacros download package and also in the lib folder in the GitHub repository.

To compile the xpl plugin (project XplPlugin.lpi), you need to make sure that the 64-bit version of the FPC compiler is used. 
