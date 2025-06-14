module Types (WorldData(..), Direction(..), GameItem(..), Position) where 

    import Graphics.Gloss (Picture)

    type Position = (Float, Float)
    

    data GameItem = GameItem Position Picture


    data WorldData = WorldData 
        { timer :: Float 
        , playerPosition :: (Float, Float)  
        , isWPressed :: Bool 
        , isAPressed :: Bool 
        , isSPressed :: Bool 
        , isDPressed :: Bool 
        , playerLastDirection :: Direction
        , worldItems :: [GameItem] 
        }

    data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Eq, Show)

