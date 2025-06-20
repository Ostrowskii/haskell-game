module Interface.Time (updateTime, drawTimer, drawPlayerPos, drawItemQuantity) where 
    
    import Graphics.Gloss ( Picture(Text, Translate, Scale) )
    import Types (WorldData(..), Position, PositionInTiles)

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


    drawPlayerPos :: PositionInTiles -> Picture 
    drawPlayerPos   playerPos = 
        let
            (xPos, yPos) = playerPos
            info =  "this: " ++ show xPos ++ " and y:"++ show yPos 
        in Translate x y $ Scale sx sy $ Text  info
        where 
        x = -400
        y = 100
        sx = 0.1
        sy = 0.1
        
    drawItemQuantity:: Int -> Picture 
    drawItemQuantity  idItem = 
        let
            info =  "itemInInventory: " ++ show idItem
        in Translate x y $ Scale sx sy $ Text  info
        where 
        x = -400
        y = 200
        sx = 0.1
        sy = 0.1
