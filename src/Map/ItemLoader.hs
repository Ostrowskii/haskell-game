module Map.ItemLoader (drawItems, loadItemImages) where

    import Types (GameItem(..))
    import Graphics.Gloss

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) pic <- itemsGame        ]

    loadItemImages :: [Picture] -> [GameItem]
    loadItemImages   itemImages  =
        [
            GameItem (64, 64) (itemImages !! 2)
        ]

