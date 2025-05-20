module Types (WorldData(..)) where 


    data WorldData = WorldData 
        { timer :: Int 
        , playerPosition :: (Float, Float)  
        , motionPerFrame :: (Float, Float)  
        }