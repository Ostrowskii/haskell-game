module Main where

  import World
  import Graphics.Gloss
  import Graphics.Gloss.Juicy (loadJuicyPNG)
  import Types (GameItem(..))


  loadImages :: [FilePath] -> IO [Picture]
  loadImages paths = do 
    maybePics <- mapM loadJuicyPNG paths 
    let pics = [pic | Just pic <- maybePics]
    return pics

  main :: IO ()
  main = do
    itemImages <- loadImages
      [  "src/img/gutiguti.png"
        ,"src/img/icecream.png"
      ]

    startGame itemImages



  -- main :: IO ()
  -- main = do
  --   --todo aprender o que é Just
  --   Just itemImage <- loadJuicyPNG "src/img/gutiguti.png"
  --   -- let scaledImageItem = scale 0.05 0.05 itemImage
  --   let initialItems = [GameItem (90,64) scaledImageItem]
  --   startGame initialItems