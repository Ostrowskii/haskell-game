    module Player.Inventory (giveItemToFriend, drawItemOnHead, handleResetInventory) where

    import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..))
    import Graphics.Gloss

    import Types ( WorldData (..), Position)

    import Map.Map (worldToTilePosition)


    drawItemOnHead :: Position -> Int -> [Picture] ->Picture
    drawItemOnHead      _           0   _ = Blank
    drawItemOnHead    playerPosition idImage allImages =
        let
            (x,y) = playerPosition
            y2 = y+40
            itemImage = allImages !! idImage

        in
        pictures [translate x y2 itemImage]


    giveItemToFriend :: WorldData -> WorldData
    giveItemToFriend    world =
        let (col, row) = worldToTilePosition (playerPosition world)
            inside = col >= 10 && col <= 12 && row >= 1 && row <= 4
        in if inside && (inventory world /= 0)
            then
                let myInventory = inventory world
                    happy = friendHappinessPercent world
                    health = friendHealthPercent world
                    (addingHappiness, addingHealth) = itemsValuesAtId myInventory
                    -- (totalHappiness, totalHealth) = (happy + addingHappiness, health + addingHealth)
                    -- (totalHappiness, totalHealth) = if myInventory == 2 then (happy + 20, health + 5) else (happy, health)
                in world { inventory = 0, friendHappinessPercent = happy + addingHappiness, friendHealthPercent = health + addingHealth }
            else world

    itemsValuesAtId :: Int -> (Float, Float)
    itemsValuesAtId     1 = (10,10)
    itemsValuesAtId     2 = (20,20)
    itemsValuesAtId     3 = (30,30)




    handleResetInventory :: Event -> WorldData -> WorldData
    handleResetInventory (EventKey (Char 'f') Down _ _) world =
        world { inventory = 0 }
    handleResetInventory _ world = world

    
