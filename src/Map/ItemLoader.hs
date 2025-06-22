    {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
    {-# HLINT ignore "Use head" #-}
    module Map.ItemLoader (drawItems, hideItemIfOnTop, pickUpItemIfOnTop, createRandomItems, spawnRandomItem, maybeSpawnNewItem) where

    import Graphics.Gloss
    import System.Random (StdGen, split, mkStdGen, randomRIO, RandomGen, randomRs, randomR)
    import Control.Monad (replicateM)

    import Types (GameItem(..), WorldData(..), Position)
    import Map.Map (tileToWorldPosition, worldToTilePosition, tilesThatItensCanSpawn)

    spawnPositions :: [Position]
    spawnPositions = map tileToWorldPosition tilesThatItensCanSpawn

    createRandomItems :: [Picture] -> [Position] -> StdGen -> Int -> ([GameItem], StdGen)
    createRandomItems itemImages occupiedPositions gen n =
        let
            availablePositions = filter (`notElem` occupiedPositions) spawnPositions
            shuffledPositions = shuffle availablePositions gen
            chosenPositions = take n shuffledPositions
            (gen1, gen2) = split gen
            itemTypes = take n $ randomRs (0, length itemImages - 1) gen1
            items = [ GameItem pos itemType (itemImages !! itemType) True
                    | (pos, itemType) <- zip chosenPositions itemTypes ]
        in (items, gen2)




    spawnRandomItem :: RandomGen g => [Picture] -> [GameItem] -> g -> (Maybe GameItem, g)
    spawnRandomItem itemImages currentItems gen =
        let
            takenPositions = [pos | GameItem pos _ _ True <- currentItems]
            availablePositions = filter (`notElem` takenPositions) spawnPositions
        in case availablePositions of
            [] -> (Nothing, gen) -- não tem lugar para spawnar
            (pos:_) -> 
                -- gera índice aleatório para escolher posição e tipo
                let (posIdx, gen1) = randomR (0, length availablePositions - 1) gen
                    posChosen = availablePositions !! posIdx
                    (typeIdx, gen2) = randomR (0, length itemImages - 1) gen1
                    newItem = GameItem posChosen typeIdx (itemImages !! typeIdx) True
                in (Just newItem, gen2)

        
    takeRandomUniquePure :: RandomGen g => Int -> [a] -> g -> ([a], g)
    takeRandomUniquePure n xs gen =
        let shuffled = shuffle xs gen
        in (take n shuffled, gen)

    shuffle :: RandomGen g => [a] -> g -> [a]
    shuffle xs gen = map snd . take (length xs) . zip (randomRs (0 :: Int, maxBound) gen) $ xs


    drawItems :: [GameItem] -> Picture
    drawItems itemsGame = pictures [translate x y pic | GameItem (x, y) _ pic True <- itemsGame]

    maybeSpawnNewItem :: Float -> [Picture] -> WorldData -> WorldData
    maybeSpawnNewItem dt itemImages world =
        let newTimer = timer world + dt
        in if newTimer >= 5
            then case spawnNewItem itemImages (worldItems world) of
                    Just newItems -> world { worldItems = newItems, timer = 0 }
                    Nothing -> world { timer = 0 }
            else world { timer = newTimer }

    spawnNewItem :: [Picture] -> [GameItem] -> Maybe [GameItem]
    spawnNewItem itemImages existingItems =
        let
            usedPositions = map (\(GameItem pos _ _ _) -> pos) existingItems
            availablePositions = filter (`notElem` usedPositions) spawnPositions
        in case availablePositions of
            [] -> Nothing
            (pos:_) ->
                let itemType = 0  -- ou qualquer lógica determinística, tipo com seed
                    item = GameItem pos itemType (itemImages !! itemType) True
                in Just (item : existingItems)

    --get the item
    pickUpItemIfOnTop :: WorldData -> WorldData
    pickUpItemIfOnTop world =
        let (updatedItems, maybeItemType) = hideItemIfOnTop (playerPosition world) (inventory world) (worldItems world)
            pickedUpInventory = case maybeItemType of
                                Just newItemType -> newItemType
                                Nothing -> inventory world
        in world { worldItems = updatedItems, inventory = pickedUpInventory }

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


