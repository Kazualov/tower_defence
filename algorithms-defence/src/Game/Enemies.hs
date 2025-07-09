module Game.Enemies where

import Graphics.Gloss
import Game.Types
import Game.Config

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy etype (x, y)) =
  translate x y $
    Pictures
      [ color (makeColorI 230 70 70 255) $
          circleSolid 20  -- base body
      , translate (-textOffset etype) (-6) $  -- center the text
          scale 0.15 0.15 $
            color black $
              Text (enemyLabel etype)
      ]

-- Position of main tower
mainTowerPos :: (Float, Float)
mainTowerPos = (colToX 0, rowToY 0)

-- Helper functions for drawEnemy
enemyLabel :: EnemyType -> String
enemyLabel (EChar c)    = [c]
enemyLabel (EInt n)     = show n
enemyLabel (EString s)  = s
textOffset :: EnemyType -> Float
textOffset (EChar _)   = 5
textOffset (EInt n)    = fromIntegral (length (show n)) * 3
textOffset (EString s) = fromIntegral (length s) * 3


-- Move one enemy
moveEnemy :: Float -> Enemy -> Enemy
moveEnemy speed (Enemy etype (x, y)) =
  let (tx, ty) = mainTowerPos
      dx = tx - x
      dy = ty - y
      dist = sqrt (dx * dx + dy * dy)
      (nx, ny) = if dist < 1
                 then (x, y)
                 else (x + speed * dx / dist, y + speed * dy / dist)
  in Enemy etype (nx, ny)

