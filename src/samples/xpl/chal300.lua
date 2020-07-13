commands2_ch300={}
--commands2_ch300[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_ch300[8]='sim/autopilot/servos_toggle' --AP toggle
commands2_ch300[32]='sim/flight_controls/landing_gear_toggle' --gear toggle
commands2_ch300[string.byte('A')]='sim/view/quick_look_2'
commands2_ch300[string.byte('S')]='sim/view/quick_look_3'
commands2_ch300[string.byte('D')]='sim/view/quick_look_4'
commands2_ch300[string.byte('3')]='sim/view/quick_look_5'
commands2_ch300[string.byte('0')]='cl300/DCP/navsrc_toggle'
--commands2_ch300[string.byte('F')]='sim/view/chase'
commands2_ch300[string.byte('F')]='sim/view/quick_look_6'
commands2_ch300[187]='sim/autopilot/heading'
commands2_ch300[189]='sim/autopilot/NAV'
commands2_ch300[221]='sim/autopilot/approach'
commands2_ch300[80]='sim/autopilot/vertical_speed'
commands2_ch300[79]='sim/autopilot/level_change'
commands2_ch300[57]='cl300/mach_hold'
commands2_ch300[13]='cl300/chklist_enter'
commands2_ch300[102]='cl300/chklist_jright'
commands2_ch300[100]='cl300/chklist_jleft'
commands2_ch300[38]='sim/general/up' -- up arrow
commands2_ch300[40]='sim/general/down' -- down arrow
commands2_ch300[37]='sim/general/left'
commands2_ch300[39]='sim/general/right'

commands1_ch300={}
commands1_ch300[string.byte('A')]='sim/ground_ops/pushback_left' 
commands1_ch300[string.byte('D')]='sim/ground_ops/pushback_right' 
commands1_ch300[string.byte('S')]='sim/ground_ops/pushback_straight' 
commands1_ch300[string.byte('W')]='sim/ground_ops/pushback_stop' 
commands1_ch300[string.byte('X')]='sim/autopilot/knots_mach_toggle'
commands1_ch300[string.byte('C')]='cl300_enh/toggle_altitude_hold'


function lb_ch300(button, direction)
    return false
end
  
function lb2_ch300(button, direction, ts)
    if (button == 21) then -- bottom red = checklist enter
        if (direction == 0) then
            lmc_xpl_command(commands2_ch300[13])
        end
        return true
    elseif (button == 20) then -- bottom - 1 blue = checklist right
        if (direction == 0) then
            lmc_xpl_command(commands2_ch300[102])
        end
        return true
    elseif (button == 9) then -- top + 1 black = checklist left
        if (direction == 0) then
            lmc_xpl_command(commands2_ch300[100])
        end
        return true
    end
    return false
end
    
function keyb1_ch300(button, direction)
    if (direction ~= 1) then
        return false
    end
    print('No challenger command for keyb1' .. button)
    com = commands1_ch300[button]
    if (com ~= nil) then
        print('Calling XPL command ' .. com)
        lmc_xpl_command(com)
    else
        return false
    end
    return true --handled
end
  
function keyb2_ch300(button, direction)
    if (direction ~= 1) then
        return false
    end
    com = commands2_ch300[button]
    if (com ~= nil) then
        print('Calling XPL command ' .. com)
        lmc_xpl_command(com)
    elseif (button == 190) then
        -- toggle parking brake
        val = lmc_get_xpl_variable('sim/flightmodel/controls/parkbrake')
        print('Parking brake is ' .. val)
        if (val == 0) then
          lmc_set_xpl_variable('sim/flightmodel/controls/parkbrake', 1)
        elseif (val == 1) then
            lmc_set_xpl_variable('sim/flightmodel/controls/parkbrake', 0)
        end
    else
        return false
    end
    return true --handled
  end

function init_ch300() 
end        


--[[ Flying notes


  

]]--