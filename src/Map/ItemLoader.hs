module Map.ItemLoader (drawItems, loadItemImages) where

    import Graphics.Gloss
    import Types (GameItem(..))

    import Map.Map (tilePositionToPixelCentered)
    

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) pic <- itemsGame        ]

    loadItemImages :: [Picture] -> [GameItem]
    loadItemImages   itemImages  =
        [
            GameItem (tilePositionToPixelCentered (3,2)) (itemImages !! 2)
        ]

