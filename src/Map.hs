module Map (drawMap) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock

    tileSizeInPixel :: Int 
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    level :: TileMap 
    level =
        [ [1,1,1,1,1,1,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,2,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,0,0,0,0,0,1]
        , [1,1,1,1,1,1,1]
        ]

    tileToBlock :: Int -> (Float, Float)  -> Picture
    tileToBlock 1 = redBlockAt
    tileToBlock 2 = blueBlockAt
    tileToBlock _ = \_ -> blank

    pixelPositionToTilePosition :: Float -> Float
    pixelPositionToTilePosition pixel = pixel / fromIntegral tileSizeInPixel

    tilePositionToPixelPosition :: Float -> Float
    tilePositionToPixelPosition tile = tile * fromIntegral tileSizeInPixel

    -- to do estudar essa parte novamente no futuro
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


