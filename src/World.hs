module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Juicy (loadJuicyPNG)

    import Types (WorldData(..), Direction(..), GameItem(..))

    import Player.Movement (handleInputMoviment, updateWorld)
    import Map.ItemLoader(drawItems, loadItemImages)
    import Map.Map (drawMap)
    import Player.Player (drawPlayer)
    import Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor)

    drawWorld ::        WorldData   -> Picture
    drawWorld             world       = pictures
        [
            drawMap,
            drawPlayer (playerPosition world),
            drawItems  (worldItems world)
        ]

    handleInput :: Event -> WorldData -> WorldData
    handleInput = handleInputMoviment


    initialState :: [GameItem] ->  WorldData
    initialState    items =     WorldData
        { timer = 0
        , playerPosition = (0,0)
        , isWPressed = False
        , isAPressed = False
        , isSPressed = False
        , isDPressed = False
        , playerLastDirection = DirectionLeft
        , worldItems = items
        }


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
