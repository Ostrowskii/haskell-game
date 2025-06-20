{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Juicy (loadJuicyPNG)

    import Types (WorldData(..), Direction(..), GameItem(..))

    import Player.Movement (handleInputMoviment, updatePlayerMoviment)
    import Player.Player (drawPlayer)
    import Map.ItemLoader(drawItems, createItems, drawSickFriend, hideItemIfOnTop, giveItemToFriend, drawItemOnHead)
    import Map.Map (drawMap, tileToWorldPosition, worldToTilePosition)
    import Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor)
    import Interface.Time (updateTime, drawTimer, drawPlayerPos, drawItemQuantity)
   



    import Graphics.Gloss.Data.ViewPort

    zoomedViewPort :: ViewPort
    zoomedViewPort = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = 1.2 } 


    drawWorld ::   [Picture] -> [Picture] ->     WorldData   -> Picture
    drawWorld       itemsImages otherImages          world       =
        let 
            (x, y) = (playerPosition world)
            (xTile, yTile) = worldToTilePosition(x,y)
        in pictures
        [
            drawMap,
            drawPlayerPos (xTile, yTile),--delete afterwards
            drawItemQuantity (inventory world), -- delete afterwards

            drawItems  (worldItems world),
            drawTimer  (timer world),
            drawSickFriend (otherImages !! 0) (otherImages !! 5), -- 0 = sick friend image
            drawPlayer (playerPosition world) [(otherImages !! 1), (otherImages !! 2), (otherImages !! 3), (otherImages !! 4)] (playerLastDirection world),
            drawItemOnHead  (playerPosition world) (inventory world) itemsImages
        ]

    handleInput :: Event -> WorldData -> WorldData
    handleInput = handleInputMoviment


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

        }

    updateWorld :: Float -> WorldData -> WorldData
    updateWorld dt world =
        let w1 = updatePlayerMoviment dt world
            w2 = updateTime dt w1
            (updatedItems, maybeItemType) = hideItemIfOnTop (playerPosition w2) (inventory w2) (worldItems w2)
            pickedUpInventory = case maybeItemType of
                                Just newItemType -> newItemType
                                Nothing -> inventory w2
            finalInventory = giveItemToFriend (playerPosition w2) pickedUpInventory
        in w2 { worldItems = updatedItems, inventory = finalInventory }

    

    startGame :: [Picture] -> [Picture] -> IO()
    startGame  itemsImages otherImages =
        play
            (InWindow "Also try Terraria!"
                (windowWidthInPixels, windowHeightInPixels)
                (windowPositionLeft, windowPositionTop))
            backgroundColor
            fps
            (initialState (createItems itemsImages))
            (drawWorld itemsImages otherImages)
            handleInput
            updateWorld
