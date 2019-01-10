gX = -1
gY = -1
gXCenter = -1
gYCenter = -1
gComSendCount = 0
cDeadZone = 50

function sendCom(direction, speed) 
  gComSendCount = gComSendCount + 1
  moveStr = 'd:'..direction..',s:'..speed..',t:500#'
  print('Com command #'..gComSendCount..': '..moveStr)
  lmc_send_to_com('FF-COM', moveStr)
end
  
function lb_ff(button, direction)
  if (button == 4) then -- set center
    if (direction == UP) then
      gXCenter = gX
      gYCenter = gY
      print('New center')
    end
    if (direction == DOWN) then
      gXCenter = -1
      gYCenter = -1
      print('Release force')
      sendCom(0,0)
    end
  else 
    if (button == 1) then
      if (direction == DOWN and gYCenter > 0) then
        gYCenter = gYCenter - 100
        print('Y minus to '..gYCenter)
        setForce()
      end
    else 
      if (button == 3) then
        if (direction == DOWN and gYCenter > 0) then
          gYCenter = gYCenter + 100
          print('Y plus to '..gYCenter)
          setForce()
        end
      else 
        --print('Callback for LB unused: button ' .. button .. ', direction '..direction)
        return false
      end
    end
  end
  return true
end
  
function setForce()
  if (gX > 0 and gY > 0 and gXCenter > 0 and gYCenter > 0) then
    currentMessage = 'Have axis [X,Y]: [' .. gX..','..gY..']'
    centerMessage = 'center is set to [' .. gXCenter..','..gYCenter..']'
    lXDelta = gXCenter - gX
    lYDelta = gYCenter - gY
    deltaMessage = 'need to move about [' .. lXDelta..','..lYDelta..']'
    print(currentMessage..', '..centerMessage..', '..deltaMessage)
    dir = 0
    -- speed diff 100 = 20, diff 1500 = 1
    absDelta = math.abs(lYDelta) - 100
    if (absDelta < 0) then absDelta = 0 end
    if (absDelta > 4000) then absDelta = 4000 end
    speed = 21 - math.floor(absDelta / 4000 * 20)
    if (lYDelta < -cDeadZone) then dir = 1 end
    if (lYDelta > cDeadZone) then dir = -1 end
    sendCom(-dir, speed)
  end
end
  
lmc_set_handler('FF-COM',function(comVal)
  print('Have data from COM: ' .. comVal)
end)

-- 1st param: device name
-- 2nd param: axis index
-- 3rd param: interval in ms
-- 4th param: minimum delta
lmc_set_axis_handler('LB',0, 100, 100, function(val, ts)
  --print('Callback for X axis - value ' .. val..', timestamp '..ts)
  gX = val
  --setForce()
end)
lmc_set_axis_handler('LB',1, 100, 100, function(val, ts)
  -- print('Callback for Y axis - value ' .. val..', timestamp '..ts)
  gY = val
  setForce()
end)
