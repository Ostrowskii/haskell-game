module Map (drawMap) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock

    tileSizeinPixel :: Int 
    tileSizeinPixel = 32

    type Tile = Int
    type TileMap = [[Tile]]

    level :: TileMap 
    level =
        [ [1,1,1,1,1,1,1]
        , [1,2,2,2,2,2,1]
        , [1,2,2,2,2,2,1]
        , [1,2,2,2,2,2,1]
        , [1,1,1,1,1,1,1]
        ]

    tileToBlock :: Int -> (Float, Float)  -> Picture
    tileToBlock 1 = redBlockAt
    tileToBlock 2 = blueBlockAt
    tileToBlock _ = \_ -> blank


    drawMap :: Picture
    drawMap = pictures
            [ tileToBlock tile (x, y)
            | (rowIndex, row) <- zip [0 ..] level
            , (colIndex, tile) <- zip [0 ..] row
            , let x = fromIntegral (colIndex * tileSizeinPixel) - xOffset
            , let y = fromIntegral (-(rowIndex * tileSizeinPixel)) + yOffset
            ]
        where
            numRows = length level
            numCols = length (head level)
            xOffset = fromIntegral (numCols * tileSizeinPixel) / 2
            yOffset = fromIntegral (numRows * tileSizeinPixel) / 2
