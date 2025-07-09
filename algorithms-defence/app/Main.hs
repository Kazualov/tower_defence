module Main where

import Graphics.Gloss
import Game.Config
import Game.Render (drawScene)

main :: IO ()
main = display
         (InWindow "Tower Defense Grid" (windowWidth, windowHeight) (100, 100))
            white
            drawScene
