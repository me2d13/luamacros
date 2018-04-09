--
-- Simple demo that shows how to map midi events 
--
-- 
lmc_print_devices()
lmc_device_set_name('LC1', 'CMD LC-1') -- Behringer CMD LC-1
lmc_print_devices()

lmc_set_midi_handler('LC1', function(status, data1, data2)

  if     (status == 176 and data1 == 16 and data2 == 65) then print('knob 1 UP')
  elseif (status == 176 and data1 == 16 and data2 == 63) then print('knob 1 DOWN')
  else 
	print('Not yet assigned: ' .. status .. ' ' .. data1 .. ' ' .. data2)
	end
end)
