clear()
--lmc_log_all();
--lmc_log_module('XPL')
--lmc_log_module('LUA')
--lmc_log_module('CFG')
lmc_device_set_name('LB', '7E9AD920')
lmc_device_set_name('LB2', '53175550')
--lmc_device_set_name('KBD1', '826BD90') this is my regular
lmc_device_set_name('KBD1', 'VID_046D')
lmc_device_set_name('KBD2', 'VID_04FC')
--lmc_device_set_name('ST', 'Saitek')
lmc_print_devices()

lmc.minimizeToTray = true

gPlane = ''
gIsHeli = false
gIsPlane = false
gPitot = false
gTranspoder = false
gRotorBrake = true
gHeadphone = true
-- roratries
gTs = {}

gLastRAltInterval = 0
gRAltCalls = {2, 5, 10, 20, 50, 100, 200, 500}

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
commands2[9]='SRS/X-Camera/Select_View_ID_4' -- tab
commands2[string.byte('G')]='sim/electrical/batteries_toggle'
commands2[string.byte('H')]='sim/electrical/generators_toggle'
commands2[string.byte('V')]='sim/lights/landing_lights_toggle'
commands2[string.byte('C')]='sim/lights/taxi_lights_toggle'
commands2[string.byte('M')]='sim/systems/avionics_toggle' -- TODO: sticker
commands2[string.byte('N')]='sim/lights/nav_lights_toggle'
commands2[string.byte('B')]='sim/lights/beacon_lights_toggle'
commands2[226]='sim/lights/beacon_lights_toggle'  -- anti col
commands2[string.byte('K')]='sim/fuel/fuel_pumps_tog'
commands2[188]='sim/flight_controls/rotor_brake_toggle' -- rotor brake
--lmc_xpl_command('sim/starters/shut_down')
--lmc_set_xpl_variable('AS350/Rotor_Brake', 0)
--lmc_set_xpl_variable('AS350/Headphone', 1)

commands2_as350={}
commands2_as350[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_as350[string.byte('8')]='sim/engines/engage_starters' --starter on


commands1={}
commands1[string.byte('R')]='sim/transponder/transponder_ident'

commands1_as350={}
commands1_as350[string.byte('P')]='AS350/SCU/Horn' --horn
commands1_as350[string.byte('J')]='AS350/Trim/Pitch_Toggle'
commands1_as350[string.byte('K')]='AS350/Trim/Roll_Toggle'
commands1_as350[string.byte('L')]='AS350/Trim/Trim_Release'
commands1_as350[string.byte('Z')]='AS350/SCU/Inst_l1' -- light1
commands1_as350[string.byte('X')]='AS350/SCU/Inst_l2' -- light2  hhhhhh


function lb_common(button, direction)
  if (button == 3) then
    lmc_xpl_command('sim/view/still_spot')
  elseif (button == 8) then
    if (direction == 1) then
      lmc_set_xpl_variable('sim/time/sim_speed', 0)
    else
      lmc_set_xpl_variable('sim/time/sim_speed', 1)
    end
  elseif (button == 10) then
    lmc_xpl_command('sim/replay/replay_toggle')
  else
    return false
  end
  return true --handled
end

function lb_as350(button, direction)
  if (button == 2) then
    if (direction == 1) then
      lmc_xpl_command_begin('AS350/Trim/Force_Trim')
    else
      lmc_xpl_command_end('AS350/Trim/Force_Trim')
    end
  else
    return false
  end
  return true
end

function lb_handler(button, direction)
  if (lb_common(button, direction)) then
    return
  elseif (gName == 'N994VA') then
    if (lb_as350(button, direction)) then
      return
    end
  end
  print('Callback for LB unused: button ' .. button .. ', direction '..direction)
end

function handle_rotary_with_cycle_value(button, direction, ts, def)
  if button == def.button or button == def.button+1 then
    if (direction == 1) then
      if gTs[def.button] == nil then gTs[def.button] = 0 end
      local tsDiff = ts - gTs[def.button]
      gTs[def.button] = ts
      local step = 1
      if (tsDiff > 300) then step = 2
      elseif (tsDiff > 200) then step = 3
      elseif (tsDiff > 100) then step = 5
      else step = 15 end
      if (button == def.button) then
        lmc_inc_xpl_variable(def.var_name, step, def.cycle, 0)
      else
        lmc_inc_xpl_variable(def.var_name, -step, 0, def.cycle)
      end
    end
    return true
  end
  return false
end

function lb2_common(button, direction, ts)
  local def = {}
  def.cycle = 360
  if (button == 0 or button == 1) then
    def.button = 0
    def.var_name = 'sim/cockpit/autopilot/heading_mag'
    return handle_rotary_with_cycle_value(button, direction, ts, def)
  elseif (button == 6 or button == 7) then
    def.button = 6
    def.var_name = 'sim/cockpit/radios/nav1_obs_degm'
    return handle_rotary_with_cycle_value(button, direction, ts, def)
  end
  return false
end

function lb2_as350(button, direction, ts)
  if direction == 1 then
    if button == 20 then lmc_xpl_command('sim/engines/engage_starters') -- on
    elseif button == 21 then lmc_xpl_command('sim/starters/shut_down') -- off
    else
      return false
    end
    return true
  end
  return false
end

function lb2_handler(button, direction, ts)
  if (lb2_as350(button, direction, ts)) then
    return
  elseif (gName == 'N994VA') then
    if (lb2_common(button, direction, ts)) then -- for now
      return
    end
  end
  print('Callback for LB unused: button ' .. button .. ', direction '..direction..', ts '..ts)
end

function keyb2_common(button, direction)
  if (direction ~= 1) then
    return
  end
  com = commands2[button]
  if (com ~= nil) then
    print('Calling XPL command ' .. com)
    lmc_xpl_command(com)
  elseif button == 74 then
    if (gPitot) then
      lmc_xpl_command('sim/ice/pitot_heat0_off')
    else
      lmc_xpl_command('sim/ice/pitot_heat0_on')
    end
    gPitot = not gPitot
  else
    return false
  end
  return true --handled
end

function keyb1_common(button, direction)
  if (direction ~= 1) then
    return
  end
  com = commands1[button]
  if (com ~= nil) then
    print('Calling XPL command ' .. com)
    lmc_xpl_command(com)
  elseif button == 52 then
    if (gTranspoder) then
      lmc_xpl_command('sim/transponder/transponder_off')
    else
      lmc_xpl_command('sim/transponder/transponder_on')
    end
    gTranspoder = not gTranspoder
  else
    return false
  end
  return true --handled
end

function keyb1_as350(button, direction)
  if (direction ~= 1) then
    return
  end
  com = commands1_as350[button]
  if (com ~= nil) then
    print('Calling XPL command ' .. com)
    lmc_xpl_command(com)
  elseif (button == 192) then --headphone: key `
    lmc_set_xpl_variable('AS350/Headphone', (gHeadphone) and 0 or 1)
    gHeadphone = not gHeadphone
  else
    return false
  end
  return true --handled
end

function keyb2_as350(button, direction)
  if (direction ~= 1) then
    return true
  end
  com = commands2_as350[button]
  if (com ~= nil) then
    print('Calling XPL command ' .. com)
    lmc_xpl_command(com)
  elseif (button == 188) then --rotor brake
    lmc_set_xpl_variable('AS350/Rotor_Brake', (gRotorBrake) and 0 or 1)
    gRotorBrake = not gRotorBrake
  else
    return false
  end
  return true --handled
end

function keyb1(button, direction)
  if (keyb1_common(button, direction)) then
    return
  elseif (gName == 'N994VA') then
    if (keyb1_as350(button, direction)) then
      return
    end
  end
  print('Callback for keyb1 unused: button ' .. button .. ', direction '..direction)
end

function keyb2(button, direction)
  if (gName == 'N994VA') then
    if (keyb2_as350(button, direction)) then
      return
    end
  end
  if (keyb2_common(button, direction)) then
    return
  end
  print('Callback for keyb2 unused: button ' .. button .. ', direction '..direction)
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
lmc_set_handler('LB',lb_handler)
lmc_set_handler('LB2',lb2_handler)
lmc_set_handler('KBD2',keyb2)
lmc_set_handler('KBD1',keyb1)
lmc_on_xpl_var_change('sim/cockpit2/gauges/indicators/radio_altimeter_height_ft_pilot', checkRAlt, 1000)

lmc_set_axis_handler('LB2',2, 200, 100, function(val, ts)
  --print('Callback for axis - value ' .. val..', timestamp '..ts)
  lmc_set_xpl_variable('AS350/Rotor_Brake', val/65535)
end)
