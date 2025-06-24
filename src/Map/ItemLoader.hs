{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Map.ItemLoader (drawItems, hideItemIfOnTop, pickUpItemIfOnTop, createRandomInitialItems, spawnItemSometimes) where

    import Graphics.Gloss
    import Types (GameItem(..), WorldData (..), Position)
    import System.Random (randomRIO, StdGen, randomR, split)

    import Map.Map (tileToWorldPosition, worldToTilePosition, spawnPositions)
    import Globals (playerSize)
    import Debug.Trace (trace)



    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) _ pic True <- itemsGame]


    createRandomInitialItems :: [Picture] -> IO GameItem
    createRandomInitialItems itemImages = do
        let availablePositions = spawnPositions
            maxIndex = length availablePositions - 1
            maxTypeIndex = length itemImages - 1

        posIndex <- randomRIO (0, maxIndex)
        typeIndex <- randomRIO (1, 3)  
        let pos = availablePositions !! posIndex
            pic = itemImages !! typeIndex

        return $ GameItem pos typeIndex pic True

    spawnItemSometimes :: [Picture] -> WorldData -> WorldData
    spawnItemSometimes itemImages world
            | itemSpawnTime world >= 5 =
                let
                    availablePositions = filter (`canSpawnAt` worldItems world) spawnPositions
                in
                    if null availablePositions
                    then world { itemSpawnTime = 0 }  -- sem espaço, apenas zera o tempo
                    else
                        let (newRng, newItem) = spawnItem (rng world) (worldItems world) itemImages
                        in world
                            { worldItems = newItem : worldItems world
                            , itemSpawnTime = 0
                            , rng = newRng
                            }
            | otherwise = world


    spawnItem :: StdGen -> [GameItem] -> [Picture] -> (StdGen, GameItem)
    spawnItem gen items itemImages =
        let
            (genPos, genType) = split gen
            availablePositions = filter (`canSpawnAt` items) spawnPositions
            (posIndex, genPos') = randomR (0, length availablePositions - 1) genPos
            spawnPosWorld = availablePositions !! posIndex

            (itemType, genType') = randomR (1, length itemImages - 1) genType
            itemPic = itemImages !! itemType

            newItem = GameItem
                { itemPosition = spawnPosWorld
                , itemType = itemType
                , itemImage = itemPic
                , itemVisible = True
                }

        in (genType', newItem)




    createRandomItemAvoidingDuplicates :: [Picture] -> [GameItem] -> IO GameItem
    createRandomItemAvoidingDuplicates itemImages existingItems = do
            let positions = spawnPositions
                maxTypeIndex = length itemImages - 1

            trySpawn positions
        where
            trySpawn [] = error "No available positions!"
            trySpawn ps = do
                index <- randomRIO (0, length ps - 1)
                let pos = ps !! index
                if canSpawnAt pos existingItems
                then do
                    itemType <- randomRIO (1, 3)
                    let pic = itemImages !! itemType
                    return $ GameItem pos itemType pic True
                else trySpawn ps


    canSpawnAt :: Position -> [GameItem] -> Bool
    canSpawnAt pos items = not $ any (\item -> itemVisible item && itemPosition item == pos) items



    --get the item
    pickUpItemIfOnTop :: WorldData -> WorldData
    pickUpItemIfOnTop world =
        let (updatedItems, maybeItemType) = hideItemIfOnTop (playerPosition world) (inventory world) (worldItems world)
            pickedUpInventory = case maybeItemType of
                                Just newItemType -> newItemType
                                Nothing -> inventory world
        in world { worldItems = updatedItems, inventory = pickedUpInventory }

    --TODO: make player box collider smaller
    hideItemIfOnTop ::  Position ->     Int ->              [GameItem] -> ([GameItem], Maybe Int)
    hideItemIfOnTop     playerPosition  currentInventory    items =
        let
            (x, y) = playerPosition
            playerTilePos  = worldToTilePosition (x, y)
            playerTilePosB = worldToTilePosition (x, y - playerSize )
            playerTilePosR = worldToTilePosition (x + playerSize, y)
            playerTilePosRB = worldToTilePosition (x + playerSize, y - playerSize)

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


