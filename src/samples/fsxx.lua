lmc_log_module('FSXX')

-- assign logical name to macro keyboard
lmc_assign_keyboard('MACROS');

-- define callback for whole device
lmc_set_handler('MACROS',function(button, direction)
  if (direction == 1) then return end  -- ignore down
  if     (button == string.byte('L')) then lmc_fs_event('LANDING_LIGHTS_TOGGLE')
  elseif (button == string.byte('N')) then lmc_fs_event('NAV1_STBY_SET', 0x11480)
  else print('Not yet assigned: ' .. button)
  end
end)