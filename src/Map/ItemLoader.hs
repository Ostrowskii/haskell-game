module Map.ItemLoader (drawItems, createItems, drawSickFriend, hideItemIfOnTop, giveItemToFriend) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData, Position)

    import Map.Map (tileToWorldPosition, tileSizeInPixel, worldToTilePosition)

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) _ pic True <- itemsGame]


    --never put an item close to another item. the 8 blocks around should be empty. walls are ok
    createItems :: [Picture]  -> [GameItem]
    createItems   itemImages   =
        [
            GameItem (tileToWorldPosition (3,2)) 2 (itemImages !! 2) True,
            GameItem (tileToWorldPosition (5,6)) 2 (itemImages !! 2) True,
            GameItem (tileToWorldPosition (7,7)) 2 (itemImages !! 2) True,
            GameItem (tileToWorldPosition (4,4)) 2 (itemImages !! 2) True
        ]

    -- study this function again in the fututre. it is a function inside a function
    hideItemIfOnTop :: Position -> Int -> [GameItem] -> ([GameItem], Maybe Int)
    hideItemIfOnTop playerPosition currentInventory items =
        let
            (x,y) = playerPosition
            playerTilePos = worldToTilePosition (x,y)
            playerTilePosB = worldToTilePosition (x,y-32)
            playerTilePosR = worldToTilePosition (x+32,y)
            playerTilePosRB = worldToTilePosition (x+32,y-32)

            processItem (GameItem pos itemId pic visible) (acc, pickedUp)
                | currentInventory /= 0 = (acc ++ [GameItem pos itemId pic visible], Nothing) -- já tem item
                | worldToTilePosition pos == playerTilePos && visible =
                    (acc ++ [GameItem pos itemId pic False], Just itemId) -- coleta item
                | otherwise =
                    (acc ++ [GameItem pos itemId pic visible], pickedUp) -- mantém como está
        in foldr processItem ([], Nothing) items

    giveItemToFriend :: Position -> Int -> Int
    giveItemToFriend playerPosition currentInventory =
        let (col, row) = worldToTilePosition playerPosition
            inside = col >= 10 && col <= 12 && row >= 1 && row <= 4
        in if inside then 0 else currentInventory



    drawSickFriend :: Picture -> Picture ->  Picture
    drawSickFriend friendImg rugImg =
        let (x,y) = tileToWorldPosition (2,10)
        --removi tapete
        in pictures [ translate x y friendImg]
 
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