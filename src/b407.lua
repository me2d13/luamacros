commands2_b407={}
commands2_b407[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_b407[string.byte('8')]='sim/engines/engage_starters' --starter on

commands1_b407={}
--commands1_b407[string.byte('P')]='b407/SCU/Horn' --horn


function lb_b407(button, direction)
    return false
end
  
function lb2_b407(button, direction, ts)
    if (button == 9 and direction == 0) then
        print('Track IR toggle')
        lmc_xpl_command('sim/view/track-ir_toggle')
        return true
    end
    return false
end
    
function keyb1_b407(button, direction)
    return false
  end
  
  function keyb2_b407(button, direction)
    return false
  end

function init_b407() 
end        