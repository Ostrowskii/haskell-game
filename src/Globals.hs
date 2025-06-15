module Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor) where


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
    backgroundColor = white
    --game SETTINGS end


    