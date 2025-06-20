{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}
module Main where

  import World
  import Graphics.Gloss
  import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)


  loadImages :: [FilePath] -> IO [Picture]
  loadImages paths = do 
    maybePics <- mapM loadJuicyPNG paths 
    let pics = [pic | Just pic <- maybePics]
    return pics
    
  loadJPGs :: [FilePath] -> IO [Picture]
  loadJPGs paths = do
    maybePics <- mapM loadJuicyJPG paths
    return [pic | Just pic <- maybePics]

  main :: IO ()
  main = do
    itemImages <- loadImages
      [ 
         "src/img/food1.png"
        ,"src/img/food1.png"
        ,"src/img/yogurt.png"
        , "src/img/food3.png"
      ]
    otherImagesPng <- loadImages 
      [   "src/img/sickfriendlonghair.png"
        , "src/img/characterUp.png"
        , "src/img/characterRight.png"
        , "src/img/characterDown.png"
        , "src/img/characterLeft.png"
      ]
    otherImagesJpg <- loadJPGs
      [ "src/img/rugone.jpg"

      ]

    startGame itemImages (otherImagesPng ++ otherImagesJpg)