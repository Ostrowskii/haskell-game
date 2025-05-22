module Moviment (handleInputMoviment, updateWorld) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Types (WorldData(..))

    playerSpeed :: Float 
    playerSpeed = 3

    handleInputMoviment :: Event -> WorldData -> WorldData 
    handleInputMoviment (EventKey (Char 'w') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy + playerSpeed)}
    handleInputMoviment (EventKey (Char 'w') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx, dy  - playerSpeed)}
    handleInputMoviment (EventKey (Char 's') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy - playerSpeed)}
    handleInputMoviment (EventKey (Char 's') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx , dy + playerSpeed)}

    handleInputMoviment (EventKey (Char 'd') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx + playerSpeed, dy)}
    handleInputMoviment (EventKey (Char 'd') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx - playerSpeed, dy)}
    handleInputMoviment (EventKey (Char 'a') Down _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx - playerSpeed, dy)}
    handleInputMoviment (EventKey (Char 'a') Up _ _) world = let (dx, dy) = motionPerFrame world in world { motionPerFrame = (dx + playerSpeed, dy)}

    handleInputMoviment _ world = world

    updateWorld :: Float -> WorldData -> WorldData 
    updateWorld _ world = 

        --to do : before moving, check if the block has colision

        
        let (x,y)       = playerPosition world 
            (dx, dy)    = motionPerFrame world
        in world { playerPosition = ( x + dx, y + dy)}
