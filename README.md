# luamacros
This software can recognize and manage multiple keyboards connected to computer with Windows OS. This is key feature to use it as macro triggerring application.
Other typical usage is for flight simulation when macro triggers can come from various sources like
* different keyboards
* game devices (joysticks)
* COM interface (arduino)
* small embeded http server
* game simulator itself - Xplane, on variable change

Macro action can be anything scripted in Lua language with some extensions
* serial communication
* Xplane simulator events (commands, data ref changes)
* http get
* OS commands

For details see http://www.hidmacros.eu/forum/

# developers guide
To compile & extend Luamacros yourself you need to
* Download & install the lates Lazarus environment: http://www.lazarus-ide.org/
* clone Luamacros code from this repository: https://github.com/me2d13/luamacros.git
* before opening luamacros project you need to install several packages into Lazarus IDE. They are located in lib subfolder. Maybe this could be somehow included into Lazarus project, but I'm not such expert - just found a way that works. Those packages are
  * Inetbase - for http communication 
  * uniqueinstance_package - to ensure only one luamacros instance is running
  * ExtraHighliters_RT - for Lua syntax highlight
  * LazSerialPort - for COM device communication
* Those packages have zip files in lib subdirectory. Just extract those zip files, open lpk file from extracted location and choose Use - Install.
* Now you should be able to open all 3 projects. These are
  * LuaMacros - the main application exe. Important: if you need to execute compiled exe don't forget to copy lua dlls from lib directory to the same place as exe file
  * XplPlugin - dll used as XPlane plugin. Note this is 64b project so you need to adapt FPC compiler accordingly
  * WinHook - dll to set global keyboard hook
  
Hope it helps, have fun, feel free to extend this guide
