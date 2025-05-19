module World (startGame) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Map (drawMap)
    import Moviment (handleMoviment, updateWorld)

    type World = (Float, Float)

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
    initialState :: World
    initialState = (0,0) 

    drawPlayer :: (Float, Float)  ->  Picture
    drawPlayer (x,y)  = translate x y (color green (circleSolid 30))

    drawWorld :: World -> Picture  
    drawWorld playerPos = pictures 
        [
            drawMap, 
            drawPlayer playerPos
        ]


    handleInput :: Event -> World -> World 
    handleInput   event world = (handleMoviment event world)





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


-- main :: IO ()
-- main = play
--          (InWindow "Lucy testando Gloss!" (windowWidthInPixels, windowHeightInPixels) (windowPositionTop, windowPositionLeft))
--          backgroundColor
--          60 -- FPS
--          initialCirclePos -- estado inicial do mundo
--          drawWorld
--          handleInput --lidar com inputs de teclado etc
--          updateWorld



