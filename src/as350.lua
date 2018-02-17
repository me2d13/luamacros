commands2_as350={}
commands2_as350[string.byte('7')]='sim/starters/shut_down' --starter off
commands2_as350[string.byte('8')]='sim/engines/engage_starters' --starter on

commands1_as350={}
commands1_as350[string.byte('P')]='AS350/SCU/Horn' --horn
commands1_as350[string.byte('J')]='AS350/Trim/Pitch_Toggle'
commands1_as350[string.byte('K')]='AS350/Trim/Roll_Toggle'
commands1_as350[string.byte('L')]='AS350/Trim/Trim_Release'
commands1_as350[string.byte('Z')]='AS350/SCU/Inst_l1' -- light1
commands1_as350[string.byte('X')]='AS350/SCU/Inst_l2' -- light2  hhhhhh

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

function init_as350() 
    lmc_set_axis_handler('LB2',2, 200, 100, function(val, ts)
        --print('Callback for axis - value ' .. val..', timestamp '..ts)
        lmc_set_xpl_variable('AS350/Rotor_Brake', val/65535)
      end)
end        