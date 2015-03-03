-- GUI thing: clear log window
clear();

-- log all modules, very verbose
lmc_log_all();

-- common LUA statement
print('This is LuaMacros. Listing detected devices...');

-- print device table
lmc_print_devices();

-- assign logical name to game device by regexp
-- 1st arg: logical name
-- 2nd arg: regexp applied on DirectX name
-- returns: Found directX name
print(lmc_device_set_name('LB', 'BU0836A'));

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
-- 3rd param: direction, 0=down, 1=up
lmc_set_handler('LB',2,0,handler)
-- now callback is active

-- add COM port (with default config values) as device
-- 1st param: logical name
-- 2nd param: port name
lmc_add_com('C3', 'COM3')

-- check device was added
lmc_print_devices()

-- define callback for whole device using inline function
-- 1st param: logical name
-- 2nd param: callback
lmc_set_handler('C3',function(comVal)
  print(comVal)
end)
