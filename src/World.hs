{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Juicy (loadJuicyPNG)
    import System.Random (StdGen, split, mkStdGen, randomRIO, RandomGen, randomRs, randomR)



    import Types (WorldData(..), Direction(..), GameItem(..))

    import Player.Movement (handleInputMoviment, updatePlayerMoviment)
    import Player.Player (drawPlayer)
    import Player.Inventory (giveItemToFriend, drawItemOnHead, handleResetInventory)
    import Map.Map (drawMap, tileToWorldPosition, worldToTilePosition)
    import Map.ItemLoader(drawItems, hideItemIfOnTop, pickUpItemIfOnTop, createRandomItems, maybeSpawnNewItem)
    import Map.Block.Decoration(drawSickFriend)
    import Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor)
    import Interface.Time (updateTime, drawInterfaces)


    drawWorld ::   [Picture] -> [Picture] ->     WorldData   -> Picture
    drawWorld       itemsImages otherImages          world       =

        pictures
        [
            drawMap,
            drawInterfaces world,
            drawItems  (worldItems world),
            drawSickFriend (otherImages !! 0) (otherImages !! 5), -- 0 = sick friend image
            drawPlayer (playerPosition world) [(otherImages !! 1), (otherImages !! 2), (otherImages !! 3), (otherImages !! 4)] (playerLastDirection world),
            drawItemOnHead  (playerPosition world) (inventory world) itemsImages
        ]

    -- handleInput :: Event -> WorldData -> WorldData
    -- handleInput = handleInputMoviment


    handleInput :: Event -> WorldData -> WorldData
    handleInput event world =
        let worldAfterMove = handleInputMoviment event world
        in handleResetInventory event worldAfterMove


    initialState :: [GameItem] ->  WorldData
    initialState    items =     WorldData
        { timer = 0
        , playerPosition = tileToWorldPosition (2,2)
        , isWPressed = False
        , isAPressed = False
        , isSPressed = False
        , isDPressed = False
        , playerLastDirection = DirectionLeft
        , worldItems = items
        , inventory = 0
        , friendHealthPercent = 50
        , friendHappinessPercent = 20
        }

    updateWorld :: [Picture] -> Float  -> WorldData -> WorldData
    updateWorld itemsImages dt world =
        let w1 = updatePlayerMoviment dt world
            w2 = updateTime dt w1
            w3 = pickUpItemIfOnTop w2
            w4 = giveItemToFriend w3
            w5 = updateFriendNeeds dt w4
            w6 = maybeSpawnNewItem dt itemsImages w5
        in w6


    startGame :: [Picture] -> [Picture] -> IO ()
    startGame itemImages otherImages = do
        let gen = mkStdGen 42
            (initialItems, _) = createRandomItems itemImages [] gen 6
            initial = initialState initialItems
        play
            (InWindow "Also try Terraria!"
                (windowWidthInPixels, windowHeightInPixels)
                (windowPositionLeft, windowPositionTop))
            backgroundColor
            fps
            initial
            (drawWorld itemImages otherImages)
            handleInput
            (updateWorld itemImages)


    updateFriendNeeds :: Float -> WorldData -> WorldData
    updateFriendNeeds dt world =
        let
            happinessLossRate = 1.0 
            healthLossRate = 0.5   

            newHappiness = max 0 (friendHappinessPercent world - dt * happinessLossRate)
            newHealth = max 0 (friendHealthPercent world - dt * healthLossRate)
        in
            world
                { friendHappinessPercent = newHappiness
                , friendHealthPercent = newHealth
                }



