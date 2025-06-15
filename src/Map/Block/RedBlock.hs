module Map.Block.RedBlock (redBlockAt, invisibleBlockAt) where
    import Graphics.Gloss
    import Map.Block.Blocks (blockSize)



    redBlockAt :: (Float, Float) -> Picture 
    redBlockAt (x,y) = translate x y (color red (rectangleSolid blockSize blockSize))

    invisibleBlockAt :: (Float, Float) -> Picture 
    invisibleBlockAt    (x,y) = translate x y (color (makeColor 1 1 1 0) (rectangleSolid blockSize blockSize))