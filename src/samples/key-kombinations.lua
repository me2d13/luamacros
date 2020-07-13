-- this example sends 3 different key combinations on 'A' key press

-- choose keyboard for macros, assign logical name
lmc_assign_keyboard('MACROS');

-- on every trigger following keys with alt + shift are send - looped with every press
keysToSend = { 'G', 'H', 'J'}
currentIndex = 1

-- send any key (letter) as keystroke
function press(key)
  lmc_send_input(string.byte(key), 0, 0) -- press
  lmc_send_input(string.byte(key), 0, 2) -- release
end

-- wraps function call with shift press
function withShift(callback)
  lmc_send_input(16, 0, 0) -- press
  callback()
  lmc_send_input(16, 0, 2) -- release
end

-- wraps function call with alt press
function withAlt(callback)
  lmc_send_input(18, 0, 0) -- press
  callback()
  lmc_send_input(18, 0, 2) -- release
end

-- wraps function call with alt press
function withCtrl(callback)
  lmc_send_input(17, 0, 0) -- press
  callback()
  lmc_send_input(17, 0, 2) -- release
end

-- define callback for 'A' key
lmc_set_handler('MACROS',65,0,function(button, direction)
  withCtrl(function() withAlt(function() withShift(function() press(keysToSend[currentIndex]) end) end) end)
  currentIndex = (currentIndex == #keysToSend and 1 or currentIndex + 1)
end) 
