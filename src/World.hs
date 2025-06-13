module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Juicy (loadJuicyPNG)

    import Types (WorldData(..), Direction(..), GameItem(..))

    import Player.Movement (handleInputMoviment, updatePlayerMoviment)
    import Player.Player (drawPlayer)
    import Map.ItemLoader(drawItems, loadItemImages)
    import Map.Map (drawMap, tilePositionToPixelCentered)
    import Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor)
    import Interface.Time (updateTime, drawTimer)

    drawWorld ::        WorldData   -> Picture
    drawWorld             world       = pictures
        [
            drawMap,
            drawPlayer (playerPosition world),
            drawItems  (worldItems world),
            drawTimer  (timer world)
        ]

    handleInput :: Event -> WorldData -> WorldData
    handleInput = handleInputMoviment


    initialState :: [GameItem] ->  WorldData
    initialState    items =     WorldData
        { timer = 0
        , playerPosition = tilePositionToPixelCentered (5,5)
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

    startGame :: [Picture] -> IO()
    startGame  itemsImages =
        play
            (InWindow "Lucy loves Gloss!"
                (windowWidthInPixels, windowHeightInPixels)
                (windowPositionLeft, windowPositionTop))
            backgroundColor
            fps
            (initialState (loadItemImages itemsImages))
            drawWorld
            handleInput
            updateWorld
