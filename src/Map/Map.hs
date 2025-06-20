{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Map.Map (drawMap, pixelPositionToBlockId, isBlockSolidAt, 
tileToWorldPosition, tileSizeInPixel, worldToTilePosition) where

    import Graphics.Gloss
    import Map.Block.RedBlock
    import Map.Block.BlueBlock
    import Map.Block.Blocks (idBlocksWithColition, lightBlueBlockAt, slightlyLighterBlueBlockAt)

    tileSizeInPixel :: Int
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    --draw map

    level :: TileMap
    level =

        [ replicate 28 1
        , [1] ++ replicate 8 4 ++ [1, 6, 5, 5] ++ replicate 5 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1, 6, 5, 5] ++ replicate 5 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1, 5, 5, 5] ++ replicate 5 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1, 5, 5, 5] ++ replicate 5 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        ,  replicate 4 1 ++ [4,4] ++ replicate 7 1 ++ [4,4] ++ replicate 7 1 ++ [4,4] ++ replicate 4 1
        , [1] ++ replicate 26 4 ++ [1]
        , [1] ++ replicate 26 4 ++ [1]
        ,  replicate 4 1 ++ [4,4] ++ replicate 7 1 ++ [4,4] ++ replicate 7 1 ++ [4,4] ++ replicate 4 1
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1] ++ replicate 8 4 ++ [1]
        , replicate 28 1
        ]
    

    tileToBlock ::  Int     -> (Float, Float)  -> Picture
    tileToBlock     1       = redBlockAt
    tileToBlock     2       = blueBlockAt
    tileToBlock     3       = invisibleBlockAt
    tileToBlock     5       = lightBlueBlockAt
    tileToBlock     6       = lightBlueBlockAt
    tileToBlock     4       = slightlyLighterBlueBlockAt
    tileToBlock     _       = \_ -> blank

    quantityLevelCol, quantityLevelRow :: Int 
    quantityLevelCol = length (head level)
    quantityLevelRow = length level

    drawMap :: Picture
    drawMap = pictures
            [ tileToBlock tile (x, y)
            | (rowIndex, row) <- zip [0 ..] level
            , (colIndex, tile) <- zip [0 ..] row
            , let x = fromIntegral (colIndex * tileSizeInPixel) - xMapCenteringValue
            , let y = fromIntegral (-(rowIndex * tileSizeInPixel)) + yMapCenteringValue
            ]

    -- is block solid????

    isBlockSolidAt ::   (Float, Float)          -> Bool
    isBlockSolidAt      (x,y) =
        let         idBlock = pixelPositionToBlockId (x, y)
                    idBlock2 = pixelPositionToBlockId (x + fromIntegral tileSizeInPixel, y)
                    idBlock3 = pixelPositionToBlockId (x, y - fromIntegral tileSizeInPixel)
                    idBlock4 = pixelPositionToBlockId (x +  fromIntegral tileSizeInPixel, y -  fromIntegral tileSizeInPixel)

        in          idBlock `elem` idBlocksWithColition 
                || idBlock2 `elem` idBlocksWithColition 
                || idBlock3 `elem` idBlocksWithColition 
                || idBlock4 `elem` idBlocksWithColition

    pixelPositionToBlockId :: (Float, Float) -> Int
    pixelPositionToBlockId (x, y) =
        let
            (xInLevel, yInLevel) = worldToTilePosition (x,y)
            ySafe = makeSureIndexInsideLevel yInLevel quantityLevelRow
            xSafe = makeSureIndexInsideLevel xInLevel quantityLevelCol
        in
            (level !! ySafe) !! xSafe

    makeSureIndexInsideLevel :: Int -> Int -> Int
    makeSureIndexInsideLevel    i       maxI = max 0 (min i (maxI - 1))

    
    -- World x tile positions

    tileToWorldPosition :: (Int, Int) -> (Float, Float)
    tileToWorldPosition (row, col) =
        let x = fromIntegral (col * tileSizeInPixel) - xMapCenteringValue
            y = fromIntegral (-(row * tileSizeInPixel)) + yMapCenteringValue
        in (x, y)

    worldToTilePosition :: (Float, Float) -> (Int, Int)
    worldToTilePosition    (x, y) = 
        let
            col = floor ((x + xMapCenteringValue) / fromIntegral tileSizeInPixel)
            row = floor ((yMapCenteringValue - y) / fromIntegral tileSizeInPixel)
        in
            (col, row)

    xMapCenteringValue, yMapCenteringValue :: Float
    xMapCenteringValue =  ((fromIntegral quantityLevelCol -1) /2) * fromIntegral tileSizeInPixel
    yMapCenteringValue =  ((fromIntegral quantityLevelRow -1) /2) * fromIntegral tileSizeInPixel
    

