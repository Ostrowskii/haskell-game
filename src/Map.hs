module Map (drawMap, pixelPositionToBlockId, isBlockSolidAt) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock
    import Block.Blocks (idBlocksWithColition)
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
    -- pixelPositionToBlockId :: (Float, Float) -> Int
    -- pixelPositionToBlockId (x, y) =
    --     let
    --         xInLevel = floor (pixelPositionToTilePosition x)
    --         yInLevel = floor (pixelPositionToTilePosition y)
    --         safeIndex i maxI = max 0 (min i (maxI - 1))  -- clamp
    --         maxRow = length level
    --         maxCol = length (head level)
    --         ySafe = safeIndex yInLevel maxRow
    --         xSafe = safeIndex xInLevel maxCol
    --     in
    --         (level !! ySafe) !! xSafe

         
    isBlockSolidAt ::   (Float, Float)          -> Bool
    isBlockSolidAt      (x,y) = 
        let         idBlock = pixelPositionToBlockId (x, y)
                    idBlock2 = pixelPositionToBlockId (x+32, y)
                    idBlock3 = pixelPositionToBlockId (x, y-32)
                    idBlock4 = pixelPositionToBlockId (x+32, y-32)
                    
        in          idBlock `elem` idBlocksWithColition || idBlock2 `elem` idBlocksWithColition || idBlock3 `elem` idBlocksWithColition || idBlock4 `elem` idBlocksWithColition

   
    pixelPositionToBlockId :: (Float, Float) -> Int
    pixelPositionToBlockId (x, y) =
        let
            numRows = length level
            numCols = length (head level)
            xOffset = tilePositionToPixelPosition (fromIntegral numCols) / 2
            yOffset = tilePositionToPixelPosition (fromIntegral numRows) / 2

            -- Corrigindo a posição para o sistema de coordenadas da matriz
            adjustedX = x + xOffset
            adjustedY = -y + yOffset

            -- Convertendo de coordenadas em pixels para posição na grade
            xInLevel = floor (pixelPositionToTilePosition adjustedX)
            yInLevel = floor (pixelPositionToTilePosition adjustedY)

            safeIndex i maxI = max 0 (min i (maxI - 1))  -- evita índice fora do array
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


