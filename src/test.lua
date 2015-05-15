-- GUI thing: clear log window
clear();
lmc_xpl_text('From LUA macros')

--lmc_log_all();
lmc_log_module('XPL')
lmc_log_spool('lmc_spool.log')
print('Version: ' .. lmc.version)
lmc.minimizeToTray = true
if (lmc.minimizeToTray) then
  print('Will minimize to tray')
else
  print('Won\'t minimize to tray')
end
--lmc_minimize()

-- log all modules, very verbose
--lmc_log_all();
--lmc_log_module('LUA')
--lmc_log_spool('lmc_spool.log')
--lmc_spawn('calc.exe')

-- common LUA statement
print('This is LuaMacros. Listing detected devices...');

-- print device table
lmc_print_devices();

-- assign logical name to game device by regexp
-- 1st arg: logical name
-- 2nd arg: regexp applied on DirectX name
-- returns: Found directX name
print(lmc_device_set_name('LB', 'BU0836A'));
print(lmc_device_set_name('KBD1', '826BD90'));
--print(lmc_device_set_name('KBD2', '1BDC3055'));
--lmc_assign_keyboard('KBD2');
-- remember 2nd param is regexp, so any unique part from that ugly keyboard system id works


-- now logical name is assigned to game device
lmc_print_devices();

--define callback function
handler = function()
  print('cus bus');
end

--test it
handler()

-- assign callback to game device, button no 2, press event
-- 1st param: logical name
-- 2nd param: button number
-- 3rd param: direction, 1=down, 0=up
-- 4th param: callback function
lmc_set_handler('LB',2,0,handler)
-- now callback is active

-- another type of device callback - assigned to whole device
-- 1st param: logical name
-- 2nd param: callback function
-- then callback function has 2 parameters and logic is in the function
-- 1st param: button number
-- 2nd param: direction, 1=down, 0=up
lmc_set_handler('LB',function(button, direction)
  print('Callback for whole joystick: button ' .. button .. ', direction '..direction)
  if (button == 3) then
    lmc_xpl_command('sim/view/still_spot')
    --lmc_xpl_command('sim/view/3d_cockpit_cmnd_look')
  end
end)

-- add COM port (with default config values) as device
-- 1st param: logical name
-- 2nd param: port name
--lmc_add_com('C3', 'COM3')
-- add COM port (with full config) as device
-- 1st param: logical name
-- 2nd param: port name
-- 3rd param: baud rate
-- 4th param: data bits (8, 7 , 6, 5)
-- 5th param: parity ('N', 'O', 'E', 'M', 'S')
-- 6th param: stop bits (1,2)
--lmc_add_com('C3', 'COM3', 1200, 6, 'E', 2)

-- check device was added
lmc_print_devices()

-- define callback for whole device using inline function
-- 1st param: logical name
-- 2nd param: callback
--[[lmc_set_handler('C3',function(comVal)
  print(comVal)
end)
]]--

-- set rc com splitter - get data buffered and separated by specific string
-- use '' to cancel seprating feature
--lmc_set_com_splitter('C3', 'a')

-- send data to COM port
--lmc_send_to_com('C3', 'ahoj')

--Xplane commands
--print(lmc_get_xpl_variable('sim/aircraft/view/acf_tailnum'))
--lmc_set_xpl_variable('sim/cockpit/radios/nav1_freq_hz', 11130)

-- Display text in Xplane
-- 1st param: text to display
-- 2nd param (optional): y-postion between 0.0 and 1.0 (default 0.3)
-- 3rd param (optional): time to display in sec (default 5)
--lmc_xpl_text('From LUA macros')
--lmc_xpl_text('From LUA macros', 0.5)
--lmc_xpl_text('From LUA macros', 0.7, 10)

-- keyboard handler
lmc_set_handler('KBD2',function(button, direction)
  print('Callback for whole keyboard 2: button ' .. button .. ', direction '..direction)
end)

lmc_set_handler('KBD2',65, 1, function()
  print('Callback for keyboard 1 key "a" down')
  lmc_send_keys('It rocks!')
end)
