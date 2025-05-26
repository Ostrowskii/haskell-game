module Map (drawMap, pixelPositionToBlockId) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock

    tileSizeInPixel :: Int 
    tileSizeInPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    --all tiles position in one place
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

    -- essa funçao é amazing. ela ta pedindo um int que a gente usa nessa propria função, mas tbm
    -- pede um (Float, Float)  que usa dentro da função que ta no retorno... tipo?? como assim??
    tileToBlock ::  Int     -> (Float, Float)  -> Picture
    tileToBlock     1       = redBlockAt
    tileToBlock     2       = blueBlockAt
    tileToBlock     _       = \_ -> blank


    


    tilePositionToPixelPosition :: Float -> Float
    tilePositionToPixelPosition tile = tile * fromIntegral tileSizeInPixel

    pixelPositionToTilePosition :: Float -> Float
    pixelPositionToTilePosition pixel = pixel / fromIntegral tileSizeInPixel

--rever essa funçao
    pixelPositionToBlockId :: (Float, Float) -> Int
    pixelPositionToBlockId (x, y) =
        let
            xInLevel = floor (pixelPositionToTilePosition x)
            yInLevel = floor (pixelPositionToTilePosition y)
            safeIndex i maxI = max 0 (min i (maxI - 1))  -- clamp
            maxRow = length level
            maxCol = length (head level)
            ySafe = safeIndex yInLevel maxRow
            xSafe = safeIndex xInLevel maxCol
        in
            (level !! ySafe) !! xSafe
            


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


