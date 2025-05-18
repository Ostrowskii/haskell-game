import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


-- constants
windowWidthInPixels, windowHeightInPixels :: Int
windowWidthInPixels = 500
windowHeightInPixels = 500

windowPositionTop, windowPositionLeft :: Int 
windowPositionTop = 100
windowPositionLeft = 200

initialCirclePos :: (Float, Float)
initialCirclePos = (-100, -50)

initialSquarePos :: (Float, Float)
initialSquarePos = (100, 50)

circleRadius, squareSize :: Float
circleRadius = 50
squareSize = 80

backgroundColor :: Color 
backgroundColor = white

-- descobrir o que é isso
type World = (Float, Float)

--display = aparentemente para somente aparecer e expor sem moviemntação

main :: IO ()
main = play
         (InWindow "Lucy testando Gloss!" (windowWidthInPixels, windowHeightInPixels) (windowPositionTop, windowPositionLeft))
         backgroundColor
         60 -- FPS
         initialCirclePos -- estado inicial do mundo
         drawWorld
         handleInput --lidar com inputs de teclado etc
         updateWorld



drawWorld :: World -> Picture
drawWorld (x, y) = pictures
  [ translate x y (color red (circleSolid circleRadius))
  , uncurry translate initialSquarePos (color blue (rectangleSolid squareSize squareSize))
  ]

handleInput :: Event -> World -> World 
handleInput (EventKey (Char 'w') Down _ _) (x, y) = (x, y + 10)
handleInput (EventKey (Char 's') Down _ _) (x, y) = (x, y - 10)
handleInput (EventKey (Char 'a') Down _ _) (x, y) = (x - 10, y)
handleInput (EventKey (Char 'd') Down _ _) (x, y) = (x + 10, y)
handleInput _ world = world

updateWorld :: Float -> World -> World
updateWorld _ pos = pos
