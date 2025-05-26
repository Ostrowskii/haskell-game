module Map (drawMap, pixelPositionToBlockId, isBlockSolidAt) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock
    import Block.Blocks (idBlocksWithColition)
    tileSizeInPixel :: Int 
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    level :: TileMap 
    level =
        [ [1,1,1,1,1,1,1]
        , [1,0,0,0,0,2,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,1,1,1,1,1,1]
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
                    idBlock2 = pixelPositionToBlockId (x+32, y)
                    idBlock3 = pixelPositionToBlockId (x, y-32)
                    idBlock4 = pixelPositionToBlockId (x+32, y-32)
                    
        in          idBlock `elem` idBlocksWithColition || idBlock2 `elem` idBlocksWithColition || idBlock3 `elem` idBlocksWithColition || idBlock4 `elem` idBlocksWithColition

   
    -- to do: review this code
    pixelPositionToBlockId :: (Float, Float) -> Int
    pixelPositionToBlockId (x, y) =
        let
            numRows = length level
            numCols = length (head level)
            xOffset = tilePositionToPixelPosition (fromIntegral numCols) / 2
            yOffset = tilePositionToPixelPosition (fromIntegral numRows) / 2

            adjustedX = x + xOffset
            adjustedY = -y + yOffset

            xInLevel = floor (pixelPositionToTilePosition adjustedX)
            yInLevel = floor (pixelPositionToTilePosition adjustedY)

            safeIndex i maxI = max 0 (min i (maxI - 1)) 
            maxRow = length level
            maxCol = length (head level)
            ySafe = safeIndex yInLevel maxRow
            xSafe = safeIndex xInLevel maxCol
        in
            (level !! ySafe) !! xSafe

    -- to do review this code
    drawMap :: Picture
    drawMap = pictures
            [ tileToBlock tile (x, y)
            -- (numero da linha, [literalmente a propria linha ]
            | (rowIndex, row) <- zip [0 ..] level
            -- (numero da coluna, literalmente o valor do bloco) 
            , (colIndex, tile) <- zip [0 ..] row
            , let x = fromIntegral (colIndex * tileSizeInPixel) - xOffset
            , let y = fromIntegral (-(rowIndex * tileSizeInPixel)) + yOffset
            ]
        where
            numRows = length level
            numCols = length (head level)
            xOffset = tilePositionToPixelPosition (fromIntegral numCols)  / 2
            yOffset = tilePositionToPixelPosition (fromIntegral numRows) / 2


