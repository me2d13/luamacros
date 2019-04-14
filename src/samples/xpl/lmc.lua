clear()
--lmc_reset()
--lmc.statistics = true
lmc.autoReload = true
--lmc.statistics = true
--lmc_log_all()
--lmc_log_module('XPL')
--lmc_log_module('LUA')
--lmc_log_module('CFG')
lmc_device_set_name('LB', '800F444553540000')
lmc_device_set_name('LB2', '8001444553540000')
--lmc_device_set_name('KBD1', '826BD90') this is my regular
lmc_device_set_name('KBD1', '34A63ED7') -- upper
lmc_device_set_name('KBD2', 'VID_04FC') -- lower
--lmc_device_set_name('ST', 'Saitek')
lmc_add_com('FF-COM', 'COM12', 9600, 8, 'N', 1) -- 9600,8,N,1, was COM9
lmc_print_devices()

lmc.minimizeToTray = true

DOWN = 1
UP = 0

scriptRoot = 'C:\\Work\\lmc\\2\\luamacros\\src\\samples\\xpl\\'

dofile(scriptRoot..'common.lua')
dofile(scriptRoot..'b407.lua')
dofile(scriptRoot..'as350.lua')
dofile(scriptRoot..'chal300.lua')
dofile(scriptRoot..'ff.lua')

gPlane = ''
gIsHeli = false
gIsPlane = false
gPitot = false
gTranspoder = false
gRotorBrake = true
gHeadphone = true
-- rotaries
gTs = {}
gRotCount = 1

gLastRAltInterval = 0
gRAltCalls = {2, 5, 10, 20, 50, 100}

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


function lb_handler(button, direction)
  if (gName == 'B40714' or gName == 'C-GELP') then
    if (lb_b407(button, direction)) then
      return
    end
  elseif (gName == 'N994VA') then
    if (lb_as350(button, direction)) then
      return
    end
  elseif (gName == 'N929BR') then
    if (lb_ch300(button, direction)) then
      return
    end
  end
  if (lb_common(button, direction)) then
    return
  end
  if (lb_ff(button, direction)) then
    return
  end

  print('Callback for LB unused: button ' .. button .. ', direction '..direction)
end

function handle_rotary_with_cycle_value(button, direction, ts, def)
  --print('Rotary #'..gRotCount..': button ' .. button .. ', direction '..direction..', ts '..ts)
  if button == def.button or button == def.button+1 then
    if (direction == 1) then
      print('Rotary #'..gRotCount..': button ' .. button .. ', direction '..direction..', ts '..ts)
      gRotCount = gRotCount + 1
      --TODO: roatry can send tripple press on one "click" so store last 4 ts and check time of 4th back
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



function lb2_handler(button, direction, ts)
  if (gName == 'B40714' or gName == 'C-GELP') then
    if (lb2_b407(button, direction, ts)) then
      return
    end
  elseif (gName == 'N994VA') then
    if (lb2_as350(button, direction, ts)) then
      return
    end
  elseif (gName == 'N929BR') then
    if (lb2_ch300(button, direction)) then
      return
    end
  end
  if (lb2_common(button, direction, ts)) then
    return
  end

  print('Callback for LB2 unused: button ' .. button .. ', direction '..direction)
end



function keyb1(button, direction)
  if (gName == 'N994VA') then
    if (keyb1_as350(button, direction)) then
      return
    end
  elseif (gName == 'B40714' or gName == 'C-GELP') then
    if (keyb1_b407(button, direction)) then
      return
    end
  elseif (gName == 'N929BR') then
    if (keyb1_ch300(button, direction)) then
      return
    end
  end
  if (keyb1_common(button, direction)) then
    return
  end
  print('Callback for keyb1 unused: button ' .. button .. ', direction '..direction)
end

function keyb2(button, direction)
  if (gName == 'N994VA') then
    if (keyb2_as350(button, direction)) then
      return
    end
  elseif (gName == 'B40714' or gName == 'C-GELP') then
    if (keyb2_b407(button, direction)) then
      return
    end
  elseif (gName == 'N929BR') then
    if (keyb2_ch300(button, direction)) then
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
    init_as350() 
  elseif (gName == 'B40714') then
    lmc_say('Bell helicopter')
    gIsPlane = false
    gIsHeli = true
    init_b407() 
  elseif (gName == 'N929BR') then
    lmc_say('Challenger plane')
    gIsPlane = true
    gIsHeli = false
    init_ch300() 
  elseif (gName == 'C-GELP') then
    lmc_say('Guimbal helicopter')
    gIsPlane = false
    gIsHeli = true
    init_b407()
  else
    lmc_say('New plane with unknown type')
    gIsPlane = false
    gIsHeli = false
  end
  print('Have new plane ' .. gName)
end

lmc_on_xpl_var_change('sim/aircraft/view/acf_tailnum', setPlane, 5000)
lmc_set_handler('LB',lb_handler)
lmc_set_handler('LB2',lb2_handler)
lmc_set_handler('KBD2',keyb2)
lmc_set_handler('KBD1',keyb1)
lmc_on_xpl_var_change('sim/cockpit2/gauges/indicators/radio_altimeter_height_ft_pilot', checkRAlt, 1000, 1)
