module Game.Shapes where

import Graphics.Gloss
import Game.Config

drawMainTower :: Picture
drawMainTower = translate (colToX 0) (rowToY 0) $
                color blue $
                rectangleSolid (cellSize * 0.8) (cellSize * 0.8)

drawTopGate = translate x y $ color red $ ThickArc 0 180 (cellSize * 2) 5
  where
    x = colToX (gridWidth `div` 2)
    y = rowToY (gridHeight - 1) + cellSize / 2


drawRightGate = translate x y $ Pictures
  [ translate (-5) 0 $ color red $ rectangleSolid 4 cellSize
  , translate ( 5) 0 $ color red $ rectangleSolid 4 cellSize
  ]
  where
    x = colToX (gridWidth - 1) + cellSize
    y = rowToY (gridHeight `div` 2)

