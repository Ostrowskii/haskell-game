{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Interface.Time (updateTime, drawInterfaces) where

    import Graphics.Gloss ( Picture(Text, Translate, Scale), pictures )
    import Text.Printf (printf)

    import Types (WorldData(..), Position, PositionInTiles)
    import Map.Map (worldToTilePosition)

    updateTime :: Float                     -> WorldData -> WorldData
    updateTime  secondsPassedSinceLastFrame world        = 
        world {timer = (timer world) + secondsPassedSinceLastFrame, 
                itemSpawnTime = (itemSpawnTime world + secondsPassedSinceLastFrame)}


    --TODO: rename document or organize this stuff in different .hs
    --TODO: adicionar zero a esquerda quando tiver um só algarismo
    drawInterfaces ::  WorldData ->Picture
    drawInterfaces  world =
        let
            (x, y) = playerPosition world
            (xTile, yTile) = worldToTilePosition (x,y)
        in pictures [
            -- drawPlayerPos (xTile, yTile),--delete afterwards
            -- drawItemQuantity (inventory world), -- delete afterwards
            drawFriendStatus world, --delete?
            drawTimer  (timer world)
        ]


    drawTimer :: Float -> Picture
    drawTimer timer =
        let hours = (round timer) `div` (60*60)
            remainderAfterHours = (round timer) `mod` (60*60)
            minutes = remainderAfterHours `div` 60
            seconds = remainderAfterHours `mod` 60
            format n = if n<10 then  '0' : show n else show n
            info = "Timer: " ++ format hours ++ ":" ++ format minutes ++ ":" ++ format seconds
        in Translate x y $ Scale sx sy $ Text info
        where
        x = -400
        y = 0
        sx = 0.1
        sy = 0.1


    drawPlayerPos :: PositionInTiles -> Picture
    drawPlayerPos   playerPos =
        let
            (xPos, yPos) = playerPos
            info =  "this: " ++ show xPos ++ " and y:"++ show yPos
        in Translate x y $ Scale sx sy $ Text  info
        where
        x = -400
        y = 100
        sx = 0.1
        sy = 0.1

    drawItemQuantity:: Int -> Picture
    drawItemQuantity  idItem =
        let
            info =  "itemInInventory: " ++ show idItem
        in Translate x y $ Scale sx sy $ Text  info
        where
        x = -400
        y = 200
        sx = 0.1
        sy = 0.1


    -- drawFriendStatus :: WorldData -> Picture
    -- drawFriendStatus world =
    --     let
    --         health = friendHealthPercent world
    --         happy = friendHappinessPercent world

    --         info = "health: " ++ printf "%.1f" health ++ "/100" ++
    --             "   happyness: " ++ printf "%.1f" happy ++ "/100"
    --     in Translate x y $ Scale sx sy $ Text info
    --     where
    --         x = -400
    --         y = 295
    --         sx = 0.2
    --         sy = 0.2

    drawFriendStatus :: WorldData -> Picture
    drawFriendStatus world =
        let
            health = friendHealthPercent world
            happy = friendHappinessPercent world

            info = "health: " ++ printf "%.0f" health ++ "/100" ++
                "   happiness: " ++ printf "%.0f" happy ++ "/100"
        in Translate x y $ Scale sx sy $ Text info
        where
            x = -400
            y = 295
            sx = 0.2
            sy = 0.2