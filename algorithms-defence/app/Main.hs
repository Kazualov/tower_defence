module Main where

import Graphics.Gloss

main :: IO ()
main = display
         (InWindow "Gloss Window" (800, 600) (100, 100))
         white
         (Circle 100)
