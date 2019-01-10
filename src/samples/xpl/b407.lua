commands2_b407={}
commands2_b407[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_b407[string.byte('8')]='sim/engines/engage_starters' --starter on
commands2_b407[70]='sim/electrical/GPU_toggle' --starter on
commands2_b407[226]='B407/overhead/toggle/anticollision_lt'
commands2_b407[74]='B407/overhead/toggle/pitot_heater'
commands2_b407[75]='B407/overhead/toggle/boostxfr_left'
commands2_b407[76]='B407/overhead/toggle/boostxfr_right'

commands1_b407={}
commands1_b407[80]='B407/horn_mute' --horn


function lb_b407(button, direction)
    return false
end
  
function lb2_b407(button, direction, ts)
    if (button == 0) then
        if (direction == DOWN and gYCenter > 0) then
            gYCenter = gYCenter - 10
            print('Y minus to '..gYCenter)
            setForce()
        end
        return true
    end
    if (button == 1) then
        if (direction == DOWN and gYCenter > 0) then
            gYCenter = gYCenter + 10
            print('Y plus to '..gYCenter)
            setForce()
        end
        return true
    end
    if (button == 9 and direction == 0) then
        print('Track IR toggle')
        lmc_xpl_command('sim/view/track-ir_toggle')
        return true
    end
    return false
end
    
function keyb1_b407(button, direction)
    com = commands1_b407[button]
    if (com ~= nil) then
        if (direction == 0) then
            print('Calling XPL command ' .. com)
            lmc_xpl_command(com)
        end
    else
        return false
    end
    return true  -- handled
  end
  
  function keyb2_b407(button, direction)
    com = commands2_b407[button]
    if (com ~= nil) then
        if (direction == 0) then
            print('Calling XPL command ' .. com)
            lmc_xpl_command(com)
        end
    else
        return false
    end
    return true  -- handled
  end

function init_b407() 
end        