module Map.Block.RedBlock (redBlockAt) where
    import Graphics.Gloss
    import Map.Block.Blocks (blockSize)



    redBlockAt :: (Float, Float) -> Picture 
    redBlockAt (x,y) = translate x y (color red (rectangleSolid blockSize blockSize))