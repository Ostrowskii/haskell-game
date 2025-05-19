module Map (drawMap) where

    import Graphics.Gloss
    import Block.RedBlock
    import Block.BlueBlock

    drawMap :: Picture
    drawMap = pictures [redBlockAt (-150, -250), blueBlockAt (100, 50), blueBlockAt (100, -50)]