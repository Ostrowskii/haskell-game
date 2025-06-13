module Player.Movement (handleInputMoviment, updateWorld) where

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
            if isDiagonal then ( x *  playerDiagonalSpeed, y * playerDiagonalSpeed) else (x * playerStraightSpeed, y * playerStraightSpeed)

    calculoArredondamento :: Float -> Float -> Float
    calculoArredondamento    x actualSpaceBetweenWallAndPlayer =  
        if actualSpaceBetweenWallAndPlayer < stopAreaBetweenWallAndPlayer then 0 else actualSpaceBetweenWallAndPlayer


    updateWorld :: Float -> WorldData -> WorldData
    updateWorld     _       world      =
        let (x, y)                                  = playerPosition        world
            (movimentOnX, movimentOnY)              = calculateMoviment     world
            (futurePositionX, futurePositionY)      = (x + movimentOnX, y + movimentOnY)

            futureBlockOnX = (futurePositionX, y)
            futureBlockOnY = (x, futurePositionY)

            xBlockHasColision = isBlockSolidAt futureBlockOnX
            yBlockHasColision = isBlockSolidAt futureBlockOnY


            --todo : criar essas variaveis somente se tiver colisao/?
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
