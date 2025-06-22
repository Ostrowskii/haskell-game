module Map.Block.Decoration (drawSickFriend) where

    import Graphics.Gloss

    import Map.Map (tileToWorldPosition)


    drawSickFriend :: Picture -> Picture ->  Picture
    drawSickFriend friendImg rugImg =
        let (x,y) = tileToWorldPosition (2,10)
        --i need to add rug and other stuff
        in pictures [ translate x y friendImg]
