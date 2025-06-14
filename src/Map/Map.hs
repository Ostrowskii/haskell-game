{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Map.Map (drawMap, pixelPositionToBlockId, isBlockSolidAt, tilePositionToPixelCentered) where

    import Graphics.Gloss
    import Map.Block.RedBlock
    import Map.Block.BlueBlock
    import Map.Block.Blocks (idBlocksWithColition)

    tileSizeInPixel :: Int
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    level :: TileMap
    level =

        [ replicate 28 1
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        ,  replicate 4 1 ++ [0,0] ++ replicate 7 1 ++ [0,0] ++ replicate 7 1 ++ [0,0] ++ replicate 4 1
        , [1] ++ replicate 26 0 ++ [1]
        , [1] ++ replicate 26 0 ++ [1]
        ,  replicate 4 1 ++ [0,0] ++ replicate 7 1 ++ [0,0] ++ replicate 7 1 ++ [0,0] ++ replicate 4 1
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1] ++ replicate 8 0 ++ [1]
        , replicate 28 1
        ]
    

    tileToBlock ::  Int     -> (Float, Float)  -> Picture
    tileToBlock     1       = redBlockAt
    tileToBlock     2       = blueBlockAt
    tileToBlock     _       = \_ -> blank

    quantityLevelCol, quantityLevelRow :: Int 
    quantityLevelCol = length (head level)
    quantityLevelRow = length level


    tilePositionToPixelPosition :: Float -> Float
    tilePositionToPixelPosition tile = tile * fromIntegral tileSizeInPixel

    pixelPositionToTilePosition :: Float -> Float
    pixelPositionToTilePosition pixel = pixel / fromIntegral tileSizeInPixel

    isBlockSolidAt ::   (Float, Float)          -> Bool
    isBlockSolidAt      (x,y) =
        let         idBlock = pixelPositionToBlockId (x, y)
                    idBlock2 = pixelPositionToBlockId (x + fromIntegral tileSizeInPixel, y)
                    idBlock3 = pixelPositionToBlockId (x, y - fromIntegral tileSizeInPixel)
                    idBlock4 = pixelPositionToBlockId (x +  fromIntegral tileSizeInPixel, y -  fromIntegral tileSizeInPixel)

        in          idBlock `elem` idBlocksWithColition || idBlock2 `elem` idBlocksWithColition || idBlock3 `elem` idBlocksWithColition || idBlock4 `elem` idBlocksWithColition

    pixelPositionToBlockId :: (Float, Float) -> Int
    pixelPositionToBlockId (x, y) =
        let

            adjustedX = x + xMapCenteringValue
            adjustedY = -y + yMapCenteringValue

            xInLevel = floor (pixelPositionToTilePosition adjustedX)
            yInLevel = floor (pixelPositionToTilePosition adjustedY)

            ySafe = makeSureIndexInsideLevel yInLevel quantityLevelRow
            xSafe = makeSureIndexInsideLevel xInLevel quantityLevelCol
        in
            (level !! ySafe) !! xSafe

    makeSureIndexInsideLevel :: Int -> Int -> Int
    makeSureIndexInsideLevel    i       maxI = max 0 (min i (maxI - 1))

    drawMap :: Picture
    drawMap = pictures
            [ tileToBlock tile (x, y)
            -- (numero da linha, [literalmente a propria linha ]
            | (rowIndex, row) <- zip [0 ..] level
            -- (numero da coluna, literalmente o valor do bloco) 
            , (colIndex, tile) <- zip [0 ..] row
            , let x = fromIntegral (colIndex * tileSizeInPixel) - xMapCenteringValue
            , let y = fromIntegral (-(rowIndex * tileSizeInPixel)) + yMapCenteringValue
            ]


    xMapCenteringValue, yMapCenteringValue :: Float
    xMapCenteringValue = tilePositionToPixelPosition (fromIntegral quantityLevelCol -1) /2
    yMapCenteringValue = tilePositionToPixelPosition (fromIntegral quantityLevelRow -1) /2


--test this function

    tilePositionToPixelCentered :: (Int, Int) -> (Float, Float)
    tilePositionToPixelCentered (row, col) =
        let x = fromIntegral (col * tileSizeInPixel) - xMapCenteringValue
            y = fromIntegral (-(row * tileSizeInPixel)) + yMapCenteringValue
        in (x, y)
