module Types (WorldData(..), Direction(..)) where 

    data WorldData = WorldData 
        { timer :: Int 
        , playerPosition :: (Float, Float)  
        , isWPressed :: Bool 
        , isAPressed :: Bool 
        , isSPressed :: Bool 
        , isDPressed :: Bool 
        , playerLastDirection :: Direction

        }

    data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Eq, Show)

