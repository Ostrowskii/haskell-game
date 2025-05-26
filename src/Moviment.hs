module Moviment (handleInputMoviment, updateWorld) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Types (WorldData(..), Direction(..))
    -- import Block.Blocks (idBlocksWithColition)
    import Map (pixelPositionToBlockId)

    playerSpeedStraight :: Float
    playerSpeedStraight = 3

    playerDiagonalSpeed :: Float
    -- playerDiagonalSpeed  playerSpeedStraight =  playerSpeedStraight * sqrt 2.0
    -- if 3 diagonal speed = 2.12132
    playerDiagonalSpeed = 2.12132

    handleInputMoviment :: Event -> WorldData -> WorldData
    handleInputMoviment (EventKey (Char 'w') Down _ _)  world  =  world { isWPressed = True, playerLastDirection = DirectionUp }
    handleInputMoviment (EventKey (Char 'w') Up _ _)    world  =  world { isWPressed = False}
    handleInputMoviment (EventKey (Char 's') Down _ _)  world  =  world { isSPressed = True, playerLastDirection = DirectionDown }
    handleInputMoviment (EventKey (Char 's') Up _ _)    world  =  world { isSPressed = False}

    handleInputMoviment (EventKey (Char 'd') Down _ _)  world  =  world { isDPressed = True, playerLastDirection = DirectionRight }
    handleInputMoviment (EventKey (Char 'd') Up _ _)    world  =  world { isDPressed = False }
    handleInputMoviment (EventKey (Char 'a') Down _ _)  world  =  world { isAPressed = True, playerLastDirection = DirectionLeft }
    handleInputMoviment (EventKey (Char 'a') Up _ _)    world  =  world { isAPressed = False }

    handleInputMoviment _ world = world

    calculateMoviment :: WorldData        -> (Float, Float)
    calculateMoviment    world =
        --aparentemente podemos definir variaveis no retorno da função para ajudar a retornar um valor
        let
            up          = isWPressed world
            down        = isSPressed world
            left        = isAPressed world
            right       = isDPressed world
            x           = (if right then 1 else 0)  + (if left then -1 else 0)
            y           = (if up then 1 else 0)     + (if down then -1 else 0)
            isDiagonal  = (x /= 0) && (y /= 0)
        in
            if isDiagonal then ( x *  playerDiagonalSpeed, y * playerDiagonalSpeed) else (x * playerSpeedStraight, y * playerSpeedStraight)

    calculateBlockNextToPlayerId :: WorldData  -> (Int,Int)
    calculateBlockNextToPlayerId    world       =
        let
            
            (x, y)                  = playerPosition world
            (dx, dy)                = calculateMoviment world
            (futureX,futureY)       = (x + dx, y + dy)
            --to do: make moviment better by cutting calculation if the player didnt move that axis
            blockXAxis              = pixelPositionToBlockId (futureX, y)
            blockYAxis              = pixelPositionToBlockId (x, futureX)

        in   (blockXAxis, blockYAxis)


    idBlocksWithColition :: [Int]
    idBlocksWithColition =
        [
            1

        ]

    canPlayerMove :: WorldData -> (Bool, Bool)
    canPlayerMove   world               =
        let     (idBlockX, idBlockY)    =   calculateBlockNextToPlayerId world
                isXBlockWalkable        =   idBlockX `elem` idBlocksWithColition
                isYBlockWalkable        =   idBlockY `elem` idBlocksWithColition
        in (isXBlockWalkable, isYBlockWalkable)


    updateWorld :: Float -> WorldData -> WorldData
    updateWorld     _       world      =

        --to do : before moving, check if the block has colision
        -- then if it has calculate how to move (to prevent extra calculations unecessary)

        let (x,y)                               = playerPosition        world
            (dx, dy)                            = calculateMoviment     world
            (canPlayerMoveX, canPlayerMoveY)    = canPlayerMove world
            actualMoviment  =   (if  canPlayerMoveX then x + dx else x, if canPlayerMoveY then y + dy else y)
        in world { playerPosition = actualMoviment}
