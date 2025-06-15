module Player.Player (drawPlayer) where

    import Graphics.Gloss
    
    import Types (Direction (..))


    drawPlayer :: (Float, Float) -> [Picture]  -> Direction ->  Picture
    drawPlayer (x,y) pixelarts direction = 
        let sprite = whichSprite direction pixelarts
           
            y2 = y +16

        in translate x y2 sprite


    whichSprite :: Direction -> [Picture] -> Picture
    whichSprite   DirectionUp       pixelArts = pixelArts !! 0
    whichSprite   DirectionRight    pixelArts = pixelArts !! 1
    whichSprite   DirectionDown     pixelArts = pixelArts !! 2
    whichSprite   _ pixelArts  =  pixelArts !! 3
