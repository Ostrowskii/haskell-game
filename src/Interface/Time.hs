module Interface.Time (updateTime, drawTimer) where 
    
    import Graphics.Gloss
    import Types (WorldData(..))

    updateTime :: Float                     -> WorldData -> WorldData
    updateTime  secondsPassedSinceLastFrame world        = world {timer = (timer world) + secondsPassedSinceLastFrame}


    --TODO: adicionar zero a esquerda quando tiver um só algorismo
    drawTimer :: Float -> Picture
    drawTimer timer = 
        let hours = (round timer) `div` (60*60)
            remainderAfterHours = (round timer) `mod` (60*60)
            minutes = remainderAfterHours `div` 60
            seconds = remainderAfterHours `mod` 60
            info = "Timer: " ++ show hours ++ ":" ++ show minutes ++ ":" ++ show seconds
        in Translate x y $ Scale sx sy $ Text info
        where 
        x = -400
        y = 0
        sx = 0.1
        sy = 0.1



