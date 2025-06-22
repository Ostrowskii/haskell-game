{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Map.ItemLoader (drawItems, hideItemIfOnTop, pickUpItemIfOnTop, createRandomItems) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData (..), Position)
    import System.Random (randomRIO)

    import Map.Map (tileToWorldPosition, worldToTilePosition, spawnPositions)



    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) _ pic True <- itemsGame]





    createRandomItems :: [Picture] -> IO [GameItem]
    createRandomItems itemImages = do
        let availablePositions = spawnPositions
            numberOfTypes = length itemImages - 1

        mapM (createRandomItem itemImages numberOfTypes) availablePositions


    createRandomItem :: [Picture] -> Int -> Position -> IO GameItem
    createRandomItem itemImages maxType pos = do
        itemTypeId <- randomRIO (0, maxType)
        let itemPic = itemImages !! itemTypeId
        return $ GameItem pos itemTypeId itemPic True




    --never put an item close to another item. the 8 blocks around should be empty. walls are ok
    --old creat items
    -- createItems :: [Picture]  -> [GameItem]
    -- createItems   itemImages   =
    --     [
    --         GameItem (tileToWorldPosition (13,2)) 1 (itemImages !! 1) True,
    --         GameItem (tileToWorldPosition (14,2)) 2 (itemImages !! 2) True,
    --         GameItem (tileToWorldPosition (14,3)) 3 (itemImages !! 3) True,
    --         GameItem (tileToWorldPosition (5,6)) 2 (itemImages !! 2) True,
    --         GameItem (tileToWorldPosition (7,7)) 3 (itemImages !! 3) True,
    --         GameItem (tileToWorldPosition (4,4)) 2 (itemImages !! 2) True
    --     ]


    --get the item
    pickUpItemIfOnTop :: WorldData -> WorldData
    pickUpItemIfOnTop world =
        let (updatedItems, maybeItemType) = hideItemIfOnTop (playerPosition world) (inventory world) (worldItems world)
            pickedUpInventory = case maybeItemType of
                                Just newItemType -> newItemType
                                Nothing -> inventory world
        in world { worldItems = updatedItems, inventory = pickedUpInventory }

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


