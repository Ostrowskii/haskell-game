module Map.Block.BlueBlock (blueBlockAt) where

    import Graphics.Gloss
    import Map.Block.Blocks
    
    blueBlockAt :: (Float, Float) -> Picture 
    blueBlockAt (x,y) = translate x y (color blue (rectangleSolid blockSize blockSize))












