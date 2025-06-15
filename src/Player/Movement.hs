module Player.Movement (handleInputMoviment, updatePlayerMoviment) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Data.Fixed (mod')

    import Types (WorldData(..), Direction(..))

    import Map.Block.Blocks (idBlocksWithColition)
    import Map.Map (pixelPositionToBlockId, isBlockSolidAt)

    playerStraightSpeed :: Float
    playerStraightSpeed = 3

    playerDiagonalSpeed :: Float
    -- playerDiagonalSpeed  playerStraightSpeed =  playerStraightSpeed * sqrt 2.0
    -- if 3 diagonal speed = 2.12132
    playerDiagonalSpeed = 2.12132

    stopAreaBetweenWallAndPlayer :: Float
    stopAreaBetweenWallAndPlayer = 0.0001

    --i am changing sprites on keyboard interactions then i could save some computer processing by not checking every frame
    handleInputMoviment :: Event -> WorldData -> WorldData
    handleInputMoviment (EventKey (Char 'w') Down _ _)  world  =  world { isWPressed = True, playerLastDirection = DirectionUp }
    handleInputMoviment (EventKey (Char 'w') Up _ _)    world  =  world { isWPressed = False, playerLastDirection = if isSPressed world then DirectionDown else playerLastDirection world}
    handleInputMoviment (EventKey (Char 's') Down _ _)  world  =  world { isSPressed = True, playerLastDirection = DirectionDown }
    handleInputMoviment (EventKey (Char 's') Up _ _)    world  =  world { isSPressed = False, playerLastDirection = if isWPressed world then DirectionUp else playerLastDirection world}

    handleInputMoviment (EventKey (Char 'd') Down _ _)  world  =  world { isDPressed = True, playerLastDirection = DirectionRight }
    handleInputMoviment (EventKey (Char 'd') Up _ _)    world  =  world { isDPressed = False, playerLastDirection = if isAPressed world then DirectionLeft else playerLastDirection world}
    handleInputMoviment (EventKey (Char 'a') Down _ _)  world  =  world { isAPressed = True, playerLastDirection = DirectionLeft }
    handleInputMoviment (EventKey (Char 'a') Up _ _)    world  =  world { isAPressed = False, playerLastDirection = if isDPressed world then DirectionRight else playerLastDirection world}

    handleInputMoviment _ world = world

    theOpositeKey :: Direction -> Direction
    theOpositeKey   DirectionUp = DirectionDown

    calculateDirectionMoviment :: WorldData     -> (Float,Float)
    calculateDirectionMoviment world =
        let
            up          = isWPressed world
            down        = isSPressed world
            left        = isAPressed world
            right       = isDPressed world
            x           = (if right then 1 else 0)  + (if left then -1 else 0)
            y           = (if up then 1 else 0)     + (if down then -1 else 0)
        in (x,y)


    calculateMoviment :: (Float, Float)        -> (Float, Float)
    calculateMoviment    (x,y) =
        let
            isDiagonal  = (x /= 0) && (y /= 0)
            moviment    = if isDiagonal then ( x *  playerDiagonalSpeed, y * playerDiagonalSpeed) else (x * playerStraightSpeed, y * playerStraightSpeed)
        in
            moviment


    calculoArredondamento :: Float -> Float -> Float
    calculoArredondamento    x actualSpaceBetweenWallAndPlayer =
        if actualSpaceBetweenWallAndPlayer < stopAreaBetweenWallAndPlayer then 0 else actualSpaceBetweenWallAndPlayer


    updatePlayerMoviment :: Float -> WorldData -> WorldData
    updatePlayerMoviment     _       world      =
        let (x, y)                                  = playerPosition        world
            (xDirection, yDirection)                = calculateDirectionMoviment world -- valores -1 0 ou 1
            (movimentOnX, movimentOnY)              = calculateMoviment     (xDirection, yDirection)
            (futurePositionX, futurePositionY)      = (x + movimentOnX, y + movimentOnY)

            futureBlockOnX = (futurePositionX, y)
            futureBlockOnY = (x, futurePositionY)

            xBlockHasColision = isBlockSolidAt futureBlockOnX
            yBlockHasColision = isBlockSolidAt futureBlockOnY


            xOnColisionWalkingStraght =  calculoArredondamento    x xActualSpaceBetweenWallAndPlayer
            yOnColisionWalkingStraght =  calculoArredondamento    y yActualSpaceBetweenWallAndPlayer

            xActualSpaceBetweenWallAndPlayer = (x `mod'` 32)
            yActualSpaceBetweenWallAndPlayer = (y `mod'` 32)

            isMovimentDiagonalAndThereisSpaceOnX = movimentOnX ==  playerDiagonalSpeed && xActualSpaceBetweenWallAndPlayer > playerDiagonalSpeed
            isPlayerAlreadyByWallOnX = xBlockHasColision && (xActualSpaceBetweenWallAndPlayer >= stopAreaBetweenWallAndPlayer)

            isMovimentDiagonalAndThereisSpaceOnY = movimentOnY ==  playerDiagonalSpeed && yActualSpaceBetweenWallAndPlayer > playerDiagonalSpeed
            isPlayerAlreadyByWallOnY = yBlockHasColision && (yActualSpaceBetweenWallAndPlayer >= stopAreaBetweenWallAndPlayer)



            calculateMovimentWithColisionOnX
              | isPlayerAlreadyByWallOnX = x
              | isMovimentDiagonalAndThereisSpaceOnX = x + movimentOnX
              | otherwise = x + xOnColisionWalkingStraght

            calculateMovimentWithColisionOnY
              | isPlayerAlreadyByWallOnY = y
              | isMovimentDiagonalAndThereisSpaceOnY = y + movimentOnY
              | otherwise = y + yOnColisionWalkingStraght



            actualMovimentOnX
                | (movimentOnX == 0) = x
                | xBlockHasColision  = calculateMovimentWithColisionOnX
                | otherwise = x + movimentOnX

            actualMovimentOnY
                | (movimentOnY == 0) = y
                | yBlockHasColision  = calculateMovimentWithColisionOnY
                | otherwise = y + movimentOnY

            actualMoviment  =   (actualMovimentOnX, actualMovimentOnY)
        in world { playerPosition = actualMoviment}
