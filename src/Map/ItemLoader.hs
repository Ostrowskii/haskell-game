module Map.ItemLoader (drawItems, loadItemImages, drawSickFriend) where

    import Graphics.Gloss
    import Types (GameItem(..))

    import Map.Map (inLevelPositionAt)
    

    drawItems :: [GameItem] -> Picture
    drawItems    itemsGame = pictures [translate x y pic | GameItem (x, y) pic <- itemsGame        ]

    loadItemImages :: [Picture] -> [GameItem]
    loadItemImages   itemImages  =
        [
            GameItem (inLevelPositionAt (3,2)) (itemImages !! 2)
        ]


    drawSickFriend :: Picture ->  Picture
    drawSickFriend pixelArt = 
        let (x,y) = inLevelPositionAt (2,10)
            y2 = y
        in pictures [translate x y2 pixelArt ]
 