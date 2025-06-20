{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Map.ItemLoader (drawItems, createItems, drawSickFriend, hideItemIfOnTop, giveItemToFriend, drawItemOnHead) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData (..), Position)

    import Map.Map (tileToWorldPosition, tileSizeInPixel, worldToTilePosition)

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) _ pic True <- itemsGame]


    --never put an item close to another item. the 8 blocks around should be empty. walls are ok
    createItems :: [Picture]  -> [GameItem]
    createItems   itemImages   =
        [
            GameItem (tileToWorldPosition (13,2)) 1 (itemImages !! 1) True,
            GameItem (tileToWorldPosition (5,6)) 2 (itemImages !! 2) True,
            GameItem (tileToWorldPosition (7,7)) 3 (itemImages !! 3) True,
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

    giveItemToFriend :: Position -> WorldData -> WorldData
    giveItemToFriend playerPosition world =
        let (col, row) = worldToTilePosition playerPosition
            inside = col >= 10 && col <= 12 && row >= 1 && row <= 4
        in if inside && (inventory world /= 0)
            then
                let myInventory = inventory world
                    happy = friendHappinessPercent world
                    health = friendHealthPercent world
                    (addingHappiness, addingHealth) = itemsValuesAtId myInventory
                    -- (totalHappiness, totalHealth) = (happy + addingHappiness, health + addingHealth)
                    -- (totalHappiness, totalHealth) = if myInventory == 2 then (happy + 20, health + 5) else (happy, health)
                in world { inventory = 0, friendHappinessPercent = happy + addingHappiness, friendHealthPercent = health + addingHealth }
            else world

    itemsValuesAtId :: Int -> (Int, Int)

    itemsValuesAtId     1 = (20,5)
    itemsValuesAtId     2 = (50,30)
    itemsValuesAtId     3 = (50,30)
    -- itemsValuesAtId     0 = (0,0)


    drawSickFriend :: Picture -> Picture ->  Picture
    drawSickFriend friendImg rugImg =
        let (x,y) = tileToWorldPosition (2,10)
        --removi tapete
        in pictures [ translate x y friendImg]
