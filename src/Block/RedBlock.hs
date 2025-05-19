module Block.RedBlock (redBlockAt) where
    import Graphics.Gloss
    import Block.Blocks



    redBlockAt :: (Float, Float) -> Picture 
    redBlockAt (x,y) = translate x y (color red (rectangleSolid blockSize blockSize))