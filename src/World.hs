module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Juicy (loadJuicyPNG)
    
    import Map (drawMap)
    import Moviment (handleInputMoviment, updateWorld)
    import Types (WorldData(..), Direction(..), GameItem(..))

    windowWidthInPixels, windowHeightInPixels :: Int
    windowWidthInPixels = 500
    windowHeightInPixels = 500

    windowPositionTop, windowPositionLeft :: Int
    windowPositionTop = 100
    windowPositionLeft = 200

    fps :: Int
    fps = 60

    backgroundColor :: Color
    backgroundColor = white


    -- player initial pos included
    initialState ::  WorldData
    initialState  = WorldData
        { timer = 0
        , playerPosition = (0,0)
        , isWPressed = False
        , isAPressed = False
        , isSPressed = False
        , isDPressed = False
        , playerLastDirection = DirectionLeft
        -- , worldItems = items
        }

    drawPlayer :: (Float, Float)  ->  Picture
    drawPlayer (x,y)  = translate x y (color green (rectangleSolid 32 32))

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures
        [
            translate x y pic | GameItem (x, y) pic <- itemsGame
        ]


    loadImages :: [Picture] -> [GameItem]
    loadImages   itemImages  = 
        [
            GameItem (64, 64) (itemImages !! 0)
        ]


    drawWorld ::    [GameItem] ->    WorldData   -> Picture
    drawWorld       itemsGame           world       = pictures
        [
            drawMap,
            drawPlayer (playerPosition world),
            drawItems itemsGame
            --rever isso?
            -- pictures [translate x y pic | GameItem (x, y) pic <- worldItems world ]
        ]


    handleInput :: Event -> WorldData -> WorldData
    handleInput   event world = handleInputMoviment event world

    --isso aqui de cima é igual a 
    --  handleInput :: Event -> WorldData -> WorldData
    --  handleInput = handleInputMoviment

    
    startGame :: [Picture] -> IO()
    startGame  itemsImages =
        play
            (InWindow "Lucy testando Gloss!"
                (windowWidthInPixels, windowHeightInPixels)
                (windowPositionLeft, windowPositionTop))
            backgroundColor
            fps
            initialState
            (drawWorld (loadImages itemsImages))
            handleInput
            updateWorld
