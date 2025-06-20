module Map.Block.Blocks (blockSize, idBlocksWithColition, lightBlueBlockAt, slightlyLighterBlueBlockAt) where


    import Graphics.Gloss

    blockSize :: Float
    blockSize = 32

    idBlocksWithColition :: [Int]
    idBlocksWithColition =
        [
            3, 2, 1, 6
        ]
    

    slightlyLighterBlueBlockAt :: (Float, Float) -> Picture 
    slightlyLighterBlueBlockAt (x, y) =  
        translate x y (color slightlyLighterBlue (rectangleSolid blockSize blockSize))


    lightBlueBlockAt :: (Float, Float) -> Picture 
    lightBlueBlockAt (x, y) =  
        translate x y (color lightBlue (rectangleSolid blockSize blockSize))

--color

    lightBlue :: Color
    lightBlue = makeColor 0.4 0.8 0.8 1.0

    slightlyLighterBlue :: Color
    slightlyLighterBlue = makeColor  0.8 0.9 1.0 1.0









