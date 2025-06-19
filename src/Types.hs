module Types (WorldData(..), Direction(..), GameItem(..), Position, PositionInTiles) where 

    import Graphics.Gloss (Picture)

    type Position = (Float, Float)
    type PositionInTiles = (Int,Int)

    data GameItem = GameItem Position Picture Bool


    data WorldData = WorldData 
        { timer :: Float 
        , playerPosition :: (Float, Float)  
        , isWPressed :: Bool 
        , isAPressed :: Bool 
        , isSPressed :: Bool 
        , isDPressed :: Bool 
        , playerLastDirection :: Direction
        , worldItems :: [GameItem]
        , inventory :: Int --item id that is in our inventory
        }

    data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Eq, Show)

