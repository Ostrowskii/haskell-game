{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Map.Map (drawMap, pixelPositionToBlockId, isBlockSolidAt, 
tileToWorldPosition, tileSizeInPixel, worldToTilePosition, spawnPositions, tilesThatItensCanSpawn) where

    import Globals (playerSize)
    import Graphics.Gloss
    import Map.Block.Blocks (idBlocksWithColition, lightBlueBlockAt, slightlyLighterBlueBlockAt, blueBlockAt, redBlockAt, invisibleBlockAt, imageAt, wallImageAt)

    tileSizeInPixel :: Int
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    --draw map

    level :: TileMap
    level =

        [  
          [11] ++ replicate 8 16 ++ [19]         ++ replicate 8 16++ [19] ++ replicate 8 16 ++ [15]
        , [11] ++ replicate 8 4 ++ [17, 6, 5, 5] ++ replicate 5 7 ++ [17] ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++ [17, 6, 5, 5] ++ replicate 5 7 ++ [17] ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++ [17, 5, 5, 5] ++ replicate 5 7 ++ [17] ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++ [17, 5, 5, 5] ++ replicate 5 7 ++ [17] ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++ [17]          ++ replicate 8 7 ++ [17] ++ replicate 8 4 ++          [15]
        , [11] ++ replicate 8 4 ++ [17]          ++ replicate 8 7 ++ [17] ++ replicate 8 4 ++          [15]
        , [11] ++ replicate 8 4 ++ [18]          ++ replicate 8 7 ++ [18] ++ replicate 8 4 ++          [15]
        , [11] ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++  [16]  ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++  [16]  ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++ [15]
        , [11] ++ replicate 26 4 ++ [15]
        , [11] ++ replicate 26 4 ++ [15]
        , [11] ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++  [19]  ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++  [19]  ++ replicate 3 16 ++ [4,4] ++ replicate 3 16 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [17] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [11] ++ replicate 8 4 ++                              [18] ++ replicate 8 4 ++ [17]                              ++ replicate 8 4 ++ [15]
        , [12 ] ++ replicate 26 13 ++ [14]
        ]
    

    tileToBlock :: Int ->   [Picture]   -> (Float, Float) -> Picture
    tileToBlock     1       otherImages = redBlockAt
    tileToBlock     2       otherImages = blueBlockAt
    tileToBlock     3       otherImages = invisibleBlockAt
    -- tileToBlock     4       otherImages = slightlyLighterBlueBlockAt --itens can spawn
    tileToBlock     5       otherImages = lightBlueBlockAt
    tileToBlock     6       otherImages = lightBlueBlockAt --with colision


    tileToBlock     4       otherImages = imageAt (otherImages !! 11) -- floor
    tileToBlock     7       otherImages = imageAt (otherImages !! 11)

    tileToBlock     11      otherImages = imageAt (otherImages !! 5) -- L
    tileToBlock     12      otherImages = imageAt (otherImages !! 6) -- LB
    tileToBlock     13      otherImages = imageAt (otherImages !! 7) -- B
    tileToBlock     14      otherImages = imageAt (otherImages !! 8) -- BR
    tileToBlock     15      otherImages = imageAt (otherImages !! 9) -- R
    tileToBlock     16      otherImages = imageAt (otherImages !! 10) -- up
    tileToBlock     17      otherImages = imageAt (otherImages !! 12) -- separate
    tileToBlock     18      otherImages = imageAt (otherImages !! 14) -- separate
    tileToBlock     19      otherImages = imageAt (otherImages !! 15) -- separate LRU

    tileToBlock     _       otherImages = \_ -> blank

    quantityLevelCol, quantityLevelRow :: Int 
    quantityLevelCol = length (head level)
    quantityLevelRow = length level

    drawMap :: [Picture] -> Picture
    drawMap     otherImages  = pictures
            [ tileToBlock  tile otherImages (x, y) 
            | (rowIndex, row) <- zip [0 ..] level
            , (colIndex, tile) <- zip [0 ..] row
            , let x = fromIntegral (colIndex * tileSizeInPixel) - xMapCenteringValue
            , let y = fromIntegral (-(rowIndex * tileSizeInPixel)) + yMapCenteringValue
            ]



    -- can i spawn an item??
    tilesThatItensCanSpawn :: [(Int, Int)]
    tilesThatItensCanSpawn =
        [ (rowIdx, colIdx)
        | (rowIdx, row) <- zip [0..] level
        , (colIdx, tile) <- zip [0..] row
        , tile == 4
        ]
    
    spawnPositions :: [(Float, Float)]
    spawnPositions = map tileToWorldPosition tilesThatItensCanSpawn



    -- is block solid????

    isBlockSolidAt ::   (Float, Float)          -> Bool
    isBlockSolidAt      (x,y) =
        let         idBlock = pixelPositionToBlockId (x, y)
                    idBlock2 = pixelPositionToBlockId (x + playerSize, y)
                    idBlock3 = pixelPositionToBlockId (x, y -  playerSize)
                    idBlock4 = pixelPositionToBlockId (x +  playerSize, y -  playerSize)

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
    

