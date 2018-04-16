commands2_ch300={}
--commands2_ch300[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_ch300[8]='sim/autopilot/servos_toggle' --AP toggle
commands2_ch300[32]='sim/flight_controls/landing_gear_toggle' --gear toggle

commands1_ch300={}
--commands1_ch300[string.byte('P')]='ch300/SCU/Horn' --horn


function lb_ch300(button, direction)
    return false
end
  
function lb2_ch300(button, direction, ts)
    return false
end
    
function keyb1_ch300(button, direction)
    return false
end
  
function keyb2_ch300(button, direction)
    if (direction ~= 1) then
        return false
    end
    com = commands2_ch300[button]
    if (com ~= nil) then
        print('Calling XPL command ' .. com)
        lmc_xpl_command(com)
    else
        return false
    end
    return true --handled
  end

function init_ch300() 
end        


--[[ Flying notes


  

]]--