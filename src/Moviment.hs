module Moviment (handleMoviment, updateWorld) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Types (WorldData(..))

    playerSpeed :: Float 
    playerSpeed = 3

    handleMoviment :: Event -> WorldData -> WorldData 
    handleMoviment (EventKey (Char 'w') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy + playerSpeed)}
    handleMoviment (EventKey (Char 'w') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx, dy  - playerSpeed)}
    handleMoviment (EventKey (Char 's') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy - playerSpeed)}
    handleMoviment (EventKey (Char 's') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy + playerSpeed)}

    handleMoviment (EventKey (Char 'd') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx + playerSpeed, dy)}
    handleMoviment (EventKey (Char 'd') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx - playerSpeed, dy)}
    handleMoviment (EventKey (Char 'a') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx - playerSpeed, dy)}
    handleMoviment (EventKey (Char 'a') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx + playerSpeed, dy)}

    handleMoviment _ world = world

    updateWorld :: Float -> WorldData -> WorldData 
    updateWorld _ world = 
        let (x,y)       = playerPosition world 
            (dx, dy)    = motionPerFrame world
        in world { playerPosition = ( x + dx, y + dy)}
