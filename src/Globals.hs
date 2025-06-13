module Globals (windowWidthInPixels, windowHeightInPixels, windowPositionTop, windowPositionLeft, fps, backgroundColor) where


    import Graphics.Gloss


        -- game SETTINGS
    windowWidthInPixels, windowHeightInPixels :: Int
    windowWidthInPixels = 890
    windowHeightInPixels = 640

    windowPositionTop, windowPositionLeft :: Int
    windowPositionTop = 100
    windowPositionLeft = 200

    fps :: Int
    fps = 60

    backgroundColor :: Color
    backgroundColor = white
    --game SETTINGS end


    