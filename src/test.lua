--z GUI thing: clear log window
clear();
lmc.autoReload = true
--lmc.statistics = true

--lmc_xpl_command('sim/view/3d_cockpit_cmnd_look')
--lmc_xpl_command('sim/view/3d_cockpit_cmnd_look')
--lmc_xpl_command('test')

--lmc_sleep(1500)
--lmc_send_input(16, 0, 0) -- press shift
--lmc_send_input(16, 0, 2) -- release shift
--lmc_send_input(67, 0, 0) -- press C
--lmc_send_input(67, 0, 2) -- release C

--lmc_xpl_text('From LUA macros')
--lmc_log_all();
--lmc_log_module('XPL')
--lmc_xpl_log_file('luamacros1.log')
--lmc_log_module('LUA')
--lmc_log_module('DX')
--lmc_log_module('CFG')
--lmc_log_module('SPE')
--lmc_log_module('HTP')
lmc_log_module('KBD')
--lmc_log_module('HOOK')
--lmc_log_spool('lmc_spool.log')
print('Version: ' .. lmc.version)
--lmc_minimize()

-- log all modules, very verbose
--lmc_log_all();
--lmc_log_spool('lmc_spool.log')
--lmc_spawn('calc.exe')

-- common LUA statement
print('This is LuaMacros. Listing detected devices...');

-- print device table
--lmc_print_devices();

-- assign logical name to game device by regexp
-- 1st arg: logical name
-- 2nd arg: regexp applied on DirectX name
-- returns: Found directX name
--print(lmc_device_set_name('LB', '7E9AD920'))
--print(lmc_device_set_name('LB2', '53175550'))
--print(lmc_device_set_name('KBD1', '3970CC3F'))
print(lmc_device_set_name('KBD2', 'FF905BA'))
--print(lmc_device_set_name('ST', 'Saitek'))
-- remember 2nd param is regexp, so any unique part from that ugly keyboard system id works
--lmc_assign_keyboard('KBD2');

--print(lmc_get_button('LB2', 9))

-- now logical name is assigned to game device
--lmc_print_devices()

--define callback function
log_handler = function(button, direction, ts)
  print('Callback for device: button ' .. button .. ', direction '..direction..', ts '..ts)
end

lmc_set_handler('KBD2', log_handler)

-- assign callback to game device, button no 2, press event
-- 1st param: logical name
-- 2nd param: button number
-- 3rd param: direction, 1=down, 0=up
-- 4th param: callback function
--lmc_set_handler('LB',2,0,log_handler)
-- now callback is active

-- another type of device callback - assigned to whole device
-- 1st param: logical name
-- 2nd param: callback function
-- then callback function has 2 parameters and logic is in the function
-- 1st param: button number
-- 2nd param: direction, 1=down, 0=up
--lmc_set_handler('LB',function(button, direction)
--  print('Callback for whole joystick: button ' .. button .. ', direction '..direction)
--  if (button == 3) then
--    lmc_xpl_command('sim/view/still_spot')
    --lmc_xpl_command('sim/view/3d_cockpit_cmnd_look')
--  end
--end)

--lmc_set_handler('LB2', log_handler)
--lmc_set_handler('ST', log_handler)

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
--lmc_print_devices()

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
--lmc_set_handler('KBD2',function(button, direction)
--  print('Callback for whole keyboard 2: button ' .. button .. ', direction '..direction)
--end)

--cleanThisKey=cleanThisKey:gsub("%(","%(")
--lmc_set_handler("LB",function(button, direction)
--      cleanThisKey=cleanThisKey:gsub("%(","%(")
--end)

--print(lmc_get_xpl_variable('sim/flightmodel/position/latitude'))

--lmc_load('E:\\lmc.lua')

--lmc_say('Flaps 45')
--lmc_say('Flaps 50')

--print(lmc_http_get('http://www.hidmacros.eu/forum/'))


--varName='sim/cockpit2/radios/actuators/adf1_frequency_hz'
--varName='sim/cockpit/radios/gps_dme_time_secs'
--lmc_on_xpl_var_change(varName,
--  function(value, count)
--    print(varName .. ' changed to ' .. value .. ' with ' .. count .. ' changes')
--    print(string.format('Value is %d', value))
--  end, 1000, 5)
--print('XPL Callback set')

--lmc_on_xpl_var_change('sim/cockpit2/radios/indicators/gps_bearing_deg_mag', function(value, count)
--     str = 'GP6_' .. string.format("%d", value) .. '|'
--      print("LUA > Arduino: " .. str .. " (GPS bearing)")
--   end, 1000, 1)
--print('XPL Callback2 set')

--lmc_http_server(12345, function(url)
  --lmc_xpl_text('From LUA macros', 0.5)
--  if url:match("^/command/.+") then
--    lmc_xpl_command(url:sub(10))
--  elseif url:match("^/get/.+") then
--    return lmc_get_xpl_variable(url:sub(6))
--  else
--    print('Unknown request ' .. url)
--  end
  --return '{"HDG" : 123}', 'application/json'
--end)

--lmc_set_axis_handler('LB2',0, 2000, 1000, function(val, ts)
--  print('Callback for axis - value ' .. val..', ts '..ts)
--end)

