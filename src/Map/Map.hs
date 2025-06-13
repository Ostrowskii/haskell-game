{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Map.Map (drawMap, pixelPositionToBlockId, isBlockSolidAt) where

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
        [
          [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]
        , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        , [0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0]
        , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,2,1,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
        , [2,0,0,1,0,0,0,0,0,1,0,0,0,0,0,2]
        , [0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
        , [0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0]
        , [0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0]
        , [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]
        , [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]
        ]

    tileToBlock ::  Int     -> (Float, Float)  -> Picture
    tileToBlock     1       = redBlockAt
    tileToBlock     2       = blueBlockAt
    tileToBlock     _       = \_ -> blank


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

            safeIndex i maxI = max 0 (min i (maxI - 1))

            maxRow = length level
            maxCol = length (head level)
            ySafe = safeIndex yInLevel maxRow
            xSafe = safeIndex xInLevel maxCol
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
    xMapCenteringValue =
        let
            numberCols = length (head level)
            xCenteringOffSet = tilePositionToPixelPosition (fromIntegral numberCols-1) /2
        in xCenteringOffSet
    yMapCenteringValue =
        let 
            numberRows = length level
            yCenteringOffSet = tilePositionToPixelPosition (fromIntegral numberRows-1) /2
        in yCenteringOffSet
