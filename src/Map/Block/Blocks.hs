module Map.Block.Blocks (blockSize, wallImageAt, idBlocksWithColition, lightBlueBlockAt, slightlyLighterBlueBlockAt, redBlockAt, invisibleBlockAt, blueBlockAt, imageAt) where


    import Graphics.Gloss

    blockSize :: Float
    blockSize = 32

    idBlocksWithColition :: [Int]
    idBlocksWithColition =
        [
            3, 2, 1, 6, 11, 12, 13, 14, 15, 16, 17, 18, 19
        ]



    
    imageAt :: Picture -> (Float, Float) -> Picture
    imageAt img (x, y)                   = translate x y img

    wallImageAt :: Picture -> (Float, Float) -> Picture
    wallImageAt     img         (x, y)                   = translate x (y+16) img


    
    redBlockAt :: (Float, Float) -> Picture 
    redBlockAt (x,y) = translate x y (color red (rectangleSolid blockSize blockSize))

    invisibleBlockAt :: (Float, Float) -> Picture 
    invisibleBlockAt    (x,y) = translate x y (color (makeColor 1 1 1 0) (rectangleSolid blockSize blockSize))

    blueBlockAt :: (Float, Float) -> Picture 
    blueBlockAt (x,y) = translate x y (color blue (rectangleSolid blockSize blockSize))


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









