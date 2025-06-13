module Main where

  import World
  import Graphics.Gloss
  import Graphics.Gloss.Juicy (loadJuicyPNG)


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
        ,"src/img/yorgut.png"
      ]

    startGame itemImages