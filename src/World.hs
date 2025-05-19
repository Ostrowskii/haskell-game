module World (startGame) where

    import Graphics.Gloss
    import Map (drawMap)

    windowWidthInPixels, windowHeightInPixels :: Int
    windowWidthInPixels = 500
    windowHeightInPixels = 500

    windowPositionTop, windowPositionLeft :: Int 
    windowPositionTop = 100
    windowPositionLeft = 200

    startGame :: IO()
    startGame = display 
                (InWindow "Lucy testando Gloss!" 
                    (windowWidthInPixels, windowHeightInPixels) 
                    (windowPositionLeft, windowPositionTop))
                white
                drawMap

