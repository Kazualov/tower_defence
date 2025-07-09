module Game.Render where

import Graphics.Gloss
import Game.Config
import Game.Shapes

drawScene :: Picture
drawScene = Pictures
  [ drawGrid
  , drawMainTower (0, 0) 2.0
  , drawArcherTower (2, 1) 1.5
  , drawCannonTower (4, 1) 1.5
  , drawSniperTower (6, 2) 1.5
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
