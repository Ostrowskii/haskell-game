import Graphics.Gloss
-- import qualified Control.Applicative as moviemntação

-- constants
windowWidthInPixels, windowHeightInPixels :: Int
windowWidthInPixels = 500
windowHeightInPixels = 500

windowPositionTop, windowPositionLeft :: Int 
windowPositionTop = 100
windowPositionLeft = 100

circleRadius, squareSize :: Float
circleRadius = 50
squareSize = 80

backgroundColor :: Color 
backgroundColor = white


--display = aparentemente para somente aparecer e expor sem moviemntação

main :: IO ()
main = animate 
          (InWindow "Lucy testando Gloss!" (windowWidthInPixels, windowHeightInPixels) 
                                            (windowPositionLeft, windowPositionTop)) 
          backgroundColor 
          updateAndShowSceneEveryFrame



          -- (pictures 
          --   [ translate (-100) (-50) (color red (circleSolid circleRadius))
          --   , translate 100 50 (color blue (rectangleSolid squareSize squareSize))
          --   ])


updateAndShowSceneEveryFrame :: Float -> Picture 
updateAndShowSceneEveryFrame time =
  pictures 
    [ translate (-100) (-50) $ color red $ circleSolid circleRadius
    , translate (100 + movement) 50 $ color blue $ rectangleSolid squareSize squareSize
    ]
  where
    movement = 50 * sin time  -- valor entre -50 e 50 que varia com o tempo
