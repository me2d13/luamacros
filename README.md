## Disclaimer
Don't use this software. Really. It's difficult to use, has bugs and author has very little time to maintain it. It was written just to connect multiple keyboards to flight simulator (X plane) to control different knobs and buttons in the plane/helicopter. Author is using it just for this purpose.

It can do many things but you need to understand what you are doing and you definitely **need to know and use lua scripting language**. If you just found luamacros by google search or saw some nice youtube videos but don't know how to write lua script it will be painful to get what you want and you will end up frustrated. Please, don't use it in this case.

There's support forum available at http://www.hidmacros.eu/forum where you can ask how things work, see example scripts etc. But please don't expect I will write lua scripts for you or teach you lua.

If you know lua or are able to learn it first, if you understand what example scripts are doing and are not afraid of spending some time playing with it, enjoy luamacros.

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
