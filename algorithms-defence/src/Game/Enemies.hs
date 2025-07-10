-- Updated Game.Enemies
module Game.Enemies where

import Graphics.Gloss
import Game.Types
import Game.Config

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy etype (x, y) _) =
  translate x y $
    Pictures
      [ color (makeColorI 255 0 255 255) $
          thickCircle 10 2  -- base body
      , translate (-textOffset etype) (-6) $  -- center the text
          scale 0.15 0.15 $
            color black $
              Text (enemyLabel etype)
      ]

-- Helper functions for drawEnemy
enemyLabel :: EnemyType -> String
enemyLabel (EChar c)    = [c]
enemyLabel (EInt n)     = show n
enemyLabel (EString s)  = s

textOffset :: EnemyType -> Float
textOffset (EChar _)   = 5
textOffset (EInt n)    = fromIntegral (length (show n)) * 3
textOffset (EString s) = fromIntegral (length s) * 3

-- Move one enemy along the path
moveEnemy :: Float -> Enemy -> Enemy
moveEnemy speed enemy@(Enemy etype (x, y) waypointIndex)
  | waypointIndex >= length pathWaypoints = enemy -- Enemy reached the end
  | otherwise = 
    let targetPos = pathWaypoints !! waypointIndex
        (tx, ty) = targetPos
        dx = tx - x
        dy = ty - y
        dist = sqrt (dx * dx + dy * dy)
        waypointReached = dist < 5.0  -- Tolerance for reaching waypoint
        
        newWaypointIndex = if waypointReached 
                          then waypointIndex + 1 
                          else waypointIndex
        
        (nx, ny) = if waypointReached && newWaypointIndex < length pathWaypoints
                   then let (nextTx, nextTy) = pathWaypoints !! newWaypointIndex
                            nextDx = nextTx - x
                            nextDy = nextTy - y
                            nextDist = sqrt (nextDx * nextDx + nextDy * nextDy)
                        in if nextDist < 1
                           then (x, y)
                           else (x + speed * nextDx / nextDist, y + speed * nextDy / nextDist)
                   else if dist < 1
                        then (x, y)
                        else (x + speed * dx / dist, y + speed * dy / dist)
    in Enemy etype (nx, ny) newWaypointIndex

-- Helper function to create a new enemy at spawn
createEnemy :: EnemyType -> Enemy
createEnemy etype = Enemy etype (head pathWaypoints) 0