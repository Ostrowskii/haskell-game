module Moviment (handleInputMoviment, updateWorld) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Data.Fixed (mod')

    import Types (WorldData(..), Direction(..))
    import Block.Blocks (idBlocksWithColition)
    import Map (pixelPositionToBlockId, isBlockSolidAt)

    playerSpeedStraight :: Float
    playerSpeedStraight = 3

    playerDiagonalSpeed :: Float
    -- playerDiagonalSpeed  playerSpeedStraight =  playerSpeedStraight * sqrt 2.0
    -- if 3 diagonal speed = 2.12132
    playerDiagonalSpeed = 2.12132

    spaceBetweenWallAndPlayer :: Float 
    spaceBetweenWallAndPlayer = 0.001

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


    calculoArredondamento :: Float -> Float
    calculoArredondamento x =  if (x `mod'` 32) > -spaceBetweenWallAndPlayer then (x `mod'` 32) -spaceBetweenWallAndPlayer else (x `mod'` 32)

    -- calculoArredondamento :: Float -> Float
    -- calculoArredondamento    xPos   = resto xPos

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
            xOnColision =  calculoArredondamento    x
            yOnColision =  calculoArredondamento    y



            isMovimentDiagonalAndThereisSpaceOnX = movimentOnX ==  playerDiagonalSpeed && xOnColision > playerDiagonalSpeed
            isPlayerAlreadyByWallOnX = xBlockHasColision && xOnColision >= spaceBetweenWallAndPlayer

            isMovimentDiagonalAndThereisSpaceOnY = movimentOnY ==  playerDiagonalSpeed && yOnColision > playerDiagonalSpeed
            isPlayerAlreadyByWallOnY = yBlockHasColision && yOnColision >= spaceBetweenWallAndPlayer




            calculateMovimentWithColisionOnX
              | isPlayerAlreadyByWallOnX = x
              | isMovimentDiagonalAndThereisSpaceOnX = x + movimentOnX 
              | otherwise = x + xOnColision 
            
            calculateMovimentWithColisionOnY
              | isPlayerAlreadyByWallOnY = y
              | isMovimentDiagonalAndThereisSpaceOnY = y + movimentOnY 
              | otherwise = y + yOnColision 



            -- movimentWithColitionOnX   =    calculateMovimentWithColisionOnX
            -- movimentWithColitionOnY   =   calculateMovimentWithColisionOnY

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


-- x + (xOnColision* signum movimentOnX) 

        --     actualMoviment  =   (if  canPlayerMoveX then futurePositionX else x, if canPlayerMoveY then futurePositionY else y)
        -- in world { playerPosition = actualMoviment}

            --            se estiver em movimento então verifique se o bloco tem colisão se não estiver em movimento, não faça calculos
            -- canPlayerMoveX      = ((movimentOnX /= 0) && not xBlockHasColision)
            -- canPlayerMoveY      = ((movimentOnY /= 0) && not yBlockHasColision)