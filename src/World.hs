module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Map (drawMap)
    import Moviment (handleInputMoviment, updateWorld)
    import Types (WorldData(..))

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
    initialState :: WorldData
    initialState = WorldData
        {
        timer = 0,
        playerPosition = (0,0) ,
        motionPerFrame = (0,0)
        }

    drawPlayer :: (Float, Float)  ->  Picture
    drawPlayer (x,y)  = translate x y (color green (circleSolid 16))

    drawWorld :: WorldData -> Picture  
    drawWorld world = pictures 
        [
            drawMap, 
            drawPlayer (playerPosition world)
        ]


    handleInput :: Event -> WorldData -> WorldData 
    handleInput   event world = (handleInputMoviment event world)

    startGame :: IO()
    startGame = play 
                (InWindow "Lucy testando Gloss!" 
                    (windowWidthInPixels, windowHeightInPixels) 
                    (windowPositionLeft, windowPositionTop))
                backgroundColor
                fps
                initialState
                drawWorld
                handleInput
                updateWorld
