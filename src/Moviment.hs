module Moviment (handleMoviment, updateWorld) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game

    type World = (Float, Float)

    handleMoviment :: Event -> World -> World 
    handleMoviment (EventKey (Char 'w') Down _ _) (x, y) = (x, y + 10)
    handleMoviment (EventKey (Char 's') Down _ _) (x, y) = (x, y - 10)
    handleMoviment (EventKey (Char 'a') Down _ _) (x, y) = (x - 10, y)
    handleMoviment (EventKey (Char 'd') Down _ _) (x, y) = (x + 10, y)
    handleMoviment _ world = world

    updateWorld :: Float -> World -> World 
    updateWorld _ pos = pos 

