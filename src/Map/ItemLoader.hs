module Map.ItemLoader (drawItems, createItems, drawSickFriend, hideItemIfOnTop, giveItemToFriend, drawItemOnHead) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData (playerPosition), Position)

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
    --TODO: make player box collider smaller
    hideItemIfOnTop :: Position -> Int -> [GameItem] -> ([GameItem], Maybe Int)
    hideItemIfOnTop playerPosition currentInventory items =
        let
            (x, y) = playerPosition
            playerTilePos  = worldToTilePosition (x, y)
            playerTilePosB = worldToTilePosition (x, y - 32)
            playerTilePosR = worldToTilePosition (x + 32, y)
            playerTilePosRB = worldToTilePosition (x + 32, y - 32)

            isTouchingItem tilePos =
                tilePos == playerTilePos  ||
                tilePos == playerTilePosB ||
                tilePos == playerTilePosR ||
                tilePos == playerTilePosRB

            processItem (GameItem pos itemId pic visible) (acc, pickedUp)
                | currentInventory /= 0 = (acc ++ [GameItem pos itemId pic visible], Nothing)
                | isTouchingItem (worldToTilePosition pos) && visible =
                    (acc ++ [GameItem pos itemId pic False], Just itemId)
                | otherwise = (acc ++ [GameItem pos itemId pic visible], pickedUp)

        in foldr processItem ([], Nothing) items





    drawItemOnHead :: Position -> Int -> [Picture] ->Picture
    drawItemOnHead      _           0   _ = Blank
    drawItemOnHead    playerPosition idImage allImages = 
        let
            (x,y) = playerPosition
            y2 = y+40
            itemImage = allImages !! idImage

        in
        pictures [translate x y2 itemImage]
    
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
 