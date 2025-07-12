-- Updated Game.Logic
module Game.Logic where

import Game.Enemies (moveEnemy, pathWaypoints)
import Game.Types
import Game.Config

updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let enemySpeed = 30
      movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      activeEnemies = filter (\(Enemy _ _ waypointIndex path) ->
        waypointIndex < length (pathWaypoints path)) movedEnemies
  in activeEnemies



