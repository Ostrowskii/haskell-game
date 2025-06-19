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

    -- study this function again in the fututre. it is a function inside a function
    hideItemIfOnTop :: Position -> [GameItem] -> [GameItem]
    hideItemIfOnTop playerPosition items =
        let
            playerTilePos = worldToTilePosition playerPosition
            updateItem (GameItem pos pic visible) =
                let itemTilePos = worldToTilePosition pos
                in if itemTilePos == playerTilePos || not visible
                then GameItem pos pic False
                else GameItem pos pic True
        in map updateItem items

    drawSickFriend :: Picture ->  Picture
    drawSickFriend pixelArt =
        let (x,y) = tileToWorldPosition (2,10)
            y2 = y
        in pictures [translate x y2 pixelArt]

    --made by accident but very interesting
    -- temporarilyHideIfOnTop :: Position -> [GameItem] -> [GameItem]
    -- temporarilyHideIfOnTop playerPosition items =
    --     let
    --         playerTilePos = worldToTilePosition playerPosition
    --         hideIfSamePos (GameItem pos pic _) =
    --             let itemTilePos = worldToTilePosition pos
    --             in if itemTilePos == playerTilePos
    --                 then GameItem pos pic False
    --                 else GameItem pos pic True
    --     in map hideIfSamePos items