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

# binary download
Download [luamacros.zip](http://www.hidmacros.eu/luamacros.zip)

# developers guide
Compiling luamacros requires some dependencies and specific free pascal version. See [Detailed guide](DevGuide.md) to get it working.
