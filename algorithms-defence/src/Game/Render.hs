module Game.Render where

import Graphics.Gloss
import Game.Config
import Game.Shapes

drawScene :: Picture
drawScene = Pictures
  [ drawGrid
  , drawMainTower
  , drawRightGate
  , drawTopGate
  ]

drawGrid :: Picture
drawGrid = Pictures
  [ translate x y $ color (greyN 0.8) $ rectangleWire cellSize cellSize
  | row <- [0 .. gridHeight - 1]
  , col <- [0 .. gridWidth - 1]
  , let x = colToX col
  , let y = rowToY row
  ]
