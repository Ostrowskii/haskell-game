module Player.Player (drawPlayer) where

    import Graphics.Gloss

    

    drawPlayer :: (Float, Float)  ->  Picture
    drawPlayer (x,y)  = translate x y (color green (rectangleSolid 32 32))
 