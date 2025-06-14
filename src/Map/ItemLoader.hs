module Map.ItemLoader (drawItems, loadItemImages, drawSickFriend) where

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


    drawSickFriend ::  Picture
    drawSickFriend = 
        let (x,y) = tilePositionToPixelCentered (2,2)
        in  translate x y (color yellow (rectangleSolid 32 32))
 