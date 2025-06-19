module Map.ItemLoader (drawItems, createItems, drawSickFriend, hideItemIfOnTop) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData, Position)

    import Map.Map (tileToWorldPosition, tileSizeInPixel, worldToTilePosition)

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) pic True <- itemsGame]

    createItems :: [Picture]  -> [GameItem]
    createItems   itemImages   =
        [
            GameItem (tileToWorldPosition (3,2)) (itemImages !! 2) True,
            GameItem (tileToWorldPosition (5,6)) (itemImages !! 2) True,
            GameItem (tileToWorldPosition (7,7)) (itemImages !! 2) True,
            GameItem (tileToWorldPosition (4,4)) (itemImages !! 2) True
        ]

    hideItemIfOnTop :: Position -> [GameItem] -> [GameItem]
    hideItemIfOnTop playerPosition items =
        let
            (x, y) = playerPosition
            (xTile, yTile) = worldToTilePosition (x,y)
        in
            if (xTile, yTile) == (4, 4)
                then [GameItem pos pic False | GameItem pos pic _ <- items]
                else items


            -- then [ 
            --         GameItem (x, y) pic False
                    
            --     | GameItem (x, y) pic True <- items
            --     ]
            -- else items
            --     then [GameItem pos pic False | GameItem pos pic _ <- items]
            --     else items




    -- this works
    -- hideItemIfOnTop :: Float -> [GameItem] -> [GameItem]
    -- hideItemIfOnTop    timer  items = 
    --     if timer > 10 then [GameItem pos pic False | GameItem pos pic _ <- items] else items




    drawSickFriend :: Picture ->  Picture
    drawSickFriend pixelArt =
        let (x,y) = tileToWorldPosition (2,10)
            y2 = y
        in pictures [translate x y2 pixelArt]





         --XX
    -- collectItemIfOnTop :: Position -> [GameItem] -> [GameItem]
    -- collectItemIfOnTop playerPos items =
    --     let (px, py) = playerPos
    --         playerTile = (pixelToTilePosition px, pixelToTilePosition py)
    --     in [ if tile == playerTile && visible 
    --             then GameItem tile pic False  
    --             else GameItem tile pic visible
    --     | GameItem tile pic visible <- items ]
        --XX


    -- makeItemInvisiable :: Int -> GameData -> GameData
    -- makeItemInvisiable    i     ItemList = 



            -- itemsOnGround = 


    -- hideItemIfOnTop :: Position -> [GameItem] -> [GameItem]
    -- hideItemIfOnTop playerPos items =
    --     let (x, y) = playerPos
    --         playerTile = (pixelToTilePosition x, pixelToTilePosition y)
    --     in [ if pixelToTilePosition xI == fst playerTile &&
    --             pixelToTilePosition yI == snd playerTile
    --         then GameItem pos pic False
    --         else GameItem pos pic visible
    --     | GameItem pos@(xI, yI) pic visible <- items
    --     ]


    -- hideItemIfOnTop :: Position  [GameItem] -> [GameItem]
    -- hideItemIfOnTop    playerPos  items = 
    --     let
    --         (x,y) = playerPos
    --         (xTile, yTile) = (pixelToTilePosition x, pixelToTilePosition y)
            
        
    -- hideItemIfOnTop :: Float -> [GameItem] -> [GameItem]
    -- hideItemIfOnTop timer items =
    --     if timer > 10
    --         then [ if (pixelToTilePosition x, pixelToTilePosition y) == (4,4)
    --                 then GameItem (x, y) pic False
    --                 else GameItem (x, y) pic visible
    --             | GameItem (x, y) pic visible <- items
    --             ]
    --         else items

