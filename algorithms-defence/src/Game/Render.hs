module Game.Render where

import Graphics.Gloss
import Game.Config
import Game.Shapes
import Game.Enemies
import Game.Types
import Game.Logic

-- This can still be used for testing, but not inside drawScene anymore
sampleEnemies :: [Enemy]
sampleEnemies =
  [ Enemy (EChar 'a') (100, 300)
  , Enemy (EInt 42) (200, 300)
  , Enemy (EString "List") (300, 300)
  ]

drawEnemies :: [Enemy] -> Picture
drawEnemies = Pictures . map drawEnemy

-- ❗️This is the corrected version for `play`
drawScene :: GameState -> Picture
drawScene gs = Pictures
  [ drawGrid
  , drawEnemies (enemies gs)   -- enemies come from GameState now
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
