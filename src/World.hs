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
    import Map.ItemLoader(drawItems, loadItemImages, drawSickFriend)
    import Map.Map (drawMap, inLevelPositionAt)
    import Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor)
    import Interface.Time (updateTime, drawTimer)



    import Graphics.Gloss.Data.ViewPort

    zoomedViewPort :: ViewPort
    zoomedViewPort = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = 1.2 } 


    drawWorld ::   [Picture] ->     WorldData   -> Picture
    drawWorld    otherImages          world       = pictures
        [
            drawMap,
            drawItems  (worldItems world),
            drawTimer  (timer world),
            drawSickFriend (otherImages !! 0), -- 0 = sick friend image
            drawPlayer (playerPosition world) [(otherImages !! 1), (otherImages !! 2), (otherImages !! 3), (otherImages !! 4)] (playerLastDirection world)
        ]

    handleInput :: Event -> WorldData -> WorldData
    handleInput = handleInputMoviment


    initialState :: [GameItem] ->  WorldData
    initialState    items =     WorldData
        { timer = 0
        , playerPosition = inLevelPositionAt (5,5)
        , isWPressed = False
        , isAPressed = False
        , isSPressed = False
        , isDPressed = False
        , playerLastDirection = DirectionLeft
        , worldItems = items

        }

    updateWorld :: Float -> WorldData -> WorldData
    updateWorld    dt       world=
        let worldAfterPlayerMoviment = updatePlayerMoviment dt world
            worldAfterUpdateTime = updateTime dt worldAfterPlayerMoviment
        in worldAfterUpdateTime

    startGame :: [Picture] -> [Picture] -> IO()
    startGame  itemsImages otherImages =
        play
            (InWindow "Lucy loves Gloss!"
                (windowWidthInPixels, windowHeightInPixels)
                (windowPositionLeft, windowPositionTop))
            backgroundColor
            fps
            (initialState (loadItemImages itemsImages))
            -- ( applyViewPortToPicture zoomedViewPort . 
            (drawWorld otherImages)
            -- )
            handleInput
            updateWorld
