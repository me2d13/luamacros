clear()
--lmc_log_all();
--lmc_log_module('XPL')
--lmc_log_module('LUA')
--lmc_log_module('CFG')
lmc_device_set_name('LB', 'BU0836A')
lmc_device_set_name('KBD1', '826BD90')
lmc_device_set_name('KBD2', '1BDC3055')
lmc_print_devices()

lmc.minimizeToTray = true

gPlane = ''
gIsHeli = false
gIsPlane = false

gLastRAltInterval = 0
gRAltCalls = {5, 10, 20, 50, 100, 200, 500}

function getRAltInterval(value)
  if value < gRAltCalls[1] then
    return 0
  end
  for i = 1,#gRAltCalls do
    if value <= gRAltCalls[i] then
      return i-1
    end
  end
  return #gRAltCalls
end

function checkRAlt(cra)
  curIndex = getRAltInterval(cra)
  if gLastRAltInterval ~= curIndex then
    if gLastRAltInterval > curIndex then
      lmc_say('' .. gRAltCalls[gLastRAltInterval])
    else
      lmc_say('' .. gRAltCalls[curIndex])
    end
    gLastRAltInterval = curIndex
  end
end

commands2={}
commands2[192]='sim/view/still_spot' -- 192 is vkey code of '`'
--commands2[string.byte('1')]='sim/view/3d_cockpit_cmnd_look'
commands2[string.byte('1')]='SRS/X-Camera/Select_View_ID_1'
commands2[string.byte('2')]='SRS/X-Camera/Select_View_ID_2'
commands2[string.byte('S')]='SRS/X-Camera/Select_View_ID_3'

function lb_common(button, direction)
  if (button == 3) then
    lmc_xpl_command('sim/view/still_spot')
  elseif (button == 2) then
    lmc_xpl_command('SRS/X-Camera/Center_Camera')
  elseif (button == 8) then
    if (direction == 1) then
      lmc_set_xpl_variable('sim/time/sim_speed', 0)
    else
      lmc_set_xpl_variable('sim/time/sim_speed', 1)
    end
  elseif (button == 10) then
    lmc_xpl_command('sim/replay/replay_toggle')
  else
    print('Callback for LB unused: button ' .. button .. ', direction '..direction)
  end
end

function keyb2_common(button, direction)
  if (direction ~= 1) then
    return
  end
  com = commands2[button]
  if (com ~= nil) then
    print('Calling XPL command ' .. com)
    lmc_xpl_command(com)
  else
    print('Callback for keyboard2 unused : button ' .. button .. ', direction '..direction)
  end
end

function setPlane(name)
  if (name == '') then
    name = lmc_get_xpl_variable('sim/aircraft/view/acf_tailnum')
  end
  if (gName == name) then
    return
  end
  gName = name
  if (gName == 'N172SP') then
    lmc_say('New plane')
    gIsPlane = true
    gIsHeli = false
  elseif (gName == 'N994VA') then
    lmc_say('New helicopter')
    gIsPlane = false
    gIsHeli = true
  else
    lmc_say('New plane with unknown type')
    gIsPlane = false
    gIsHeli = false
  end
  print('Have new plane ' .. gName)
end

lmc_on_xpl_var_change('sim/aircraft/view/acf_tailnum', setPlane)
lmc_set_handler('LB',lb_common)
lmc_set_handler('KBD2',keyb2_common)
lmc_on_xpl_var_change('sim/cockpit2/gauges/indicators/radio_altimeter_height_ft_pilot', checkRAlt, 1000)
