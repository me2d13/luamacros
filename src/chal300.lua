commands2_ch300={}
commands2_ch300[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_ch300[string.byte('8')]='sim/engines/engage_starters' --starter on

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
    return false
  end

function init_ch300() 
end        