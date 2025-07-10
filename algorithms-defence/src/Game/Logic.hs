-- Updated Game.Logic
module Game.Logic where

import Game.Enemies (moveEnemy)
import Game.Types

updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let enemySpeed = 50
      movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      -- Filter out enemies that have reached the end
      activeEnemies = filter (\(Enemy _ _ waypointIndex) -> waypointIndex < length pathWaypoints) movedEnemies
  in activeEnemies