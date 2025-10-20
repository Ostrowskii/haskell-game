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
            y2 = y+48
            x2 = x -4
            itemImage = allImages !! idImage

        in
        pictures [translate x2 y2 itemImage]


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
                in world { inventory = 0, friendHappinessPercent = happy + addingHappiness, friendHealthPercent = health + addingHealth }
            else world

    itemsValuesAtId :: Int -> (Float, Float)
    itemsValuesAtId     1 = (10,10)
    itemsValuesAtId     2 = (20,20)
    itemsValuesAtId     3 = (30,30)
    itemsValuesAtId     4 = (40,-20) --hamburguer
    itemsValuesAtId     5 = (30,-10) --pizza
    itemsValuesAtId     6 = (20,-10) --milshake




    handleResetInventory :: Event -> WorldData -> WorldData
    handleResetInventory (EventKey (Char 'f') Down _ _) world =
        world { inventory = 0 }
    handleResetInventory _ world = world

    
