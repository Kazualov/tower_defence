module Game.Enemies where

import Graphics.Gloss
import Game.Types
import Game.Config
import qualified Game.Types as Game

-- Теперь createEnemy принимает путь явно
createEnemy :: EnemyType -> Game.Types.Path -> Enemy
createEnemy etype path =
  let spawnPoint = case path of
        Upper -> head upperPathWaypoints
        Lower -> head lowerPathWaypoints
  in Enemy etype spawnPoint 0 path

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy etype (x, y) _ _) =
  translate x y $
    Pictures
      [ color (makeColorI 255 0 255 255) $
          thickCircle 8 1
      , translate (-textOffset etype) (-6) $
          scale 0.15 0.15 $
            color black $
              Text (enemyLabel etype)
      ]

enemyLabel :: EnemyType -> String
enemyLabel (EChar c)    = [c]
enemyLabel (EInt n)     = show n
enemyLabel (EString s)  = s

textOffset :: EnemyType -> Float
textOffset (EChar _)   = 5
textOffset (EInt n)    = fromIntegral (length (show n)) * 3
textOffset (EString s) = fromIntegral (length s) * 3

pathWaypoints :: Game.Types.Path -> [Position]
pathWaypoints Upper = upperPathWaypoints
pathWaypoints Lower = lowerPathWaypoints

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy speed enemy@(Enemy etype (x, y) waypointIndex path)
  | waypointIndex >= length waypoints = enemy
  | otherwise =
      let (tx, ty) = waypoints !! waypointIndex
          dx = tx - x
          dy = ty - y
          dist = sqrt (dx * dx + dy * dy)
          waypointReached = dist < 5.0
          newWaypointIndex = if waypointReached then waypointIndex + 1 else waypointIndex
          (nx, ny) =
            if waypointReached && newWaypointIndex < length waypoints
              then let (nextTx, nextTy) = waypoints !! newWaypointIndex
                       nextDx = nextTx - x
                       nextDy = nextTy - y
                       nextDist = sqrt (nextDx * nextDx + nextDy * nextDy)
                   in if nextDist < 1 then (x, y)
                      else (x + speed * nextDx / nextDist, y + speed * nextDy / nextDist)
              else if dist < 1 then (x, y)
              else (x + speed * dx / dist, y + speed * dy / dist)
      in Enemy etype (nx, ny) newWaypointIndex path
  where waypoints = pathWaypoints path
