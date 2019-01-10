commands2={}
commands2[192]='sim/view/still_spot' -- 192 is vkey code of '`'
commands2[string.byte('1')]='sim/view/3d_cockpit_cmnd_look'
--commands2[string.byte('1')]='SRS/X-Camera/Select_View_ID_1'
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

gInnerViews = {'sim/view/default_view','sim/view/quick_look_1','sim/view/quick_look_2','sim/view/quick_look_3','sim/view/quick_look_4','sim/view/quick_look_5','sim/view/quick_look_6'}
gInnerViewsIndex = 1
gInnerViewsSentTs = 0

gOuterViews = {'sim/view/still_spot','sim/view/tower','sim/view/runway','sim/view/circle', 'sim/view/chase'}
gOuterViewsIndex = 1
gOuterViewsSentTs = 0


commands1={}
commands1[string.byte('R')]='sim/transponder/transponder_ident'

function lb_common(button, direction)
    if (button == 8) then
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
  
  
function lb2_common(button, direction, ts)
    --print('Lb2 common button'..button..', direction '..direction..', ts '..ts)
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
    -- views
  if (direction == 1) then
    local lInnerDelta = 0
    if (button == 18) then
        lInnerDelta = 1
    elseif (button == 19) then
        lInnerDelta = -1
    end
    if (lInnerDelta ~= 0 and ts - gInnerViewsSentTs > 400) then -- 400ms ignore repeats
        if (lInnerDelta < 0 and gInnerViewsIndex <= 1) then return true end
        if (lInnerDelta > 0 and gInnerViewsIndex >= #gInnerViews) then return true end
        gInnerViewsIndex = gInnerViewsIndex + lInnerDelta
        local lCommand = gInnerViews[gInnerViewsIndex]
        print('Sending '..lCommand)
        lmc_xpl_command(lCommand)
        lmc_xpl_text(lCommand)
        gInnerViewsSentTs = ts
        return true
    end
    local lOuterDelta = 0
    if (button == 12) then
        lOuterDelta = 1
    elseif (button == 13) then
        lOuterDelta = -1
    end
    if (lOuterDelta ~= 0 and ts - gOuterViewsSentTs > 400) then -- 400ms ignore repeats
        if (lOuterDelta < 0 and gOuterViewsIndex <= 1) then return true end
        if (lOuterDelta > 0 and gOuterViewsIndex >= #gOuterViews) then return true end
        gOuterViewsIndex = gOuterViewsIndex + lOuterDelta
        local lCommand = gOuterViews[gOuterViewsIndex]
        print('Sending '..lCommand)
        lmc_xpl_command(lCommand)
        lmc_xpl_text(lCommand)
        gOuterViewsSentTs = ts
        return true
    end
  end
  return false
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
    