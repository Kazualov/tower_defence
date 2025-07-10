-- Updated Game.Logic
module Game.Logic where

import Game.Enemies (moveEnemy)
import Game.Types
import Game.Config
import Data.List (mapAccumL)

towerDamage :: Int
towerDamage = 10

towerRange :: Float
towerRange = 100  -- radius in pixels

applyTowerDamage :: [Tower] -> [Enemy] -> [Enemy]
applyTowerDamage towers enemies =
  [ if any (inRange pos) towers
      then enemy { health = health enemy - towerDamage }
      else enemy
  | enemy@(Enemy _ pos _ hp) <- enemies, hp > 0
  ]
  where
    inRange :: Position -> Tower -> Bool
    inRange (ex, ey) (_, (tx, ty)) =
      sqrt ((ex - tx)^2 + (ey - ty)^2) <= towerRange


updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let enemySpeed = 50
      movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      -- Filter out enemies that have reached the end
      activeEnemies = filter (\(Enemy _ _ waypointIndex _) -> waypointIndex < length pathWaypoints) movedEnemies
  in activeEnemies

  