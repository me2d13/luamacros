## Disclaimer
Don't use this software. Really. It's difficult to use, has bugs and the author has very little time to maintain it. It was written just to connect multiple keyboards to a flight simulator (X-Plane) to control different knobs and buttons in the plane/helicopter. The author is using it just for this purpose.

It can do many things, but you need to understand what you are doing and you definitely **need to know and use the Lua scripting language**. If you just found LuaMacros by a Google search or saw some nice YouTube videos, but don't know how to write a Lua script it will be painful to get what you want, and you will end up frustrated. Please, don't use it in this case.

There's a support forum available at http://www.hidmacros.eu/forum where you can ask how things work, see example scripts, etc. But please don't expect I will write Lua scripts for you or teach you Lua.

If you know Lua or are able to learn it first, if you understand what example scripts are doing and are not afraid of spending some time playing with it, enjoy LuaMacros.

# LuaMacros
This software can recognize and manage multiple keyboards connected to a computer with Windows OS. This is the key feature to use it as macro-triggerring application.
Other typical usage is for flight simulation when macro triggers can come from various sources like
* different keyboards
* game devices (joysticks)
* COM interface (Arduino)
* small embeded HTTP server
* game simulator itself - X-Plane, on variable change

Macro action can be anything scripted in the Lua language with some extensions
* serial communication
* X-Plane simulator events (commands, data ref changes)
* HTTP GET
* OS commands

For details, see http://www.hidmacros.eu/forum/

# Binary download
Download [luamacros.zip](http://www.hidmacros.eu/luamacros.zip)

# Developers guide
Compiling LuaMacros requires some dependencies and a specific free Pascal version. See [the detailed guide](DevGuide.md) to get it working.
