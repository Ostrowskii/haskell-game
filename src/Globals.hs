module Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, 
backgroundColor, jpgImagesStart) where


    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss


        -- game SETTINGS
    windowWidthInPixels, windowHeightInPixels :: Int
    windowWidthInPixels = 890
    windowHeightInPixels = 640
    -- windowWidthInPixels = 1600
    -- windowHeightInPixels = 1200

    windowPositionTop, windowPositionLeft :: Int
    windowPositionTop = 100
    windowPositionLeft = 200

    fps :: Int
    fps = 60

    backgroundColor :: Color
    backgroundColor = black
    --game SETTINGS end

    jpgImagesStart :: Int 
    jpgImagesStart = 5
    

    --this is usefull
    zoomedViewPort :: ViewPort
    zoomedViewPort = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = 1.2 } 