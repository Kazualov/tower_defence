module Game.Logic where

import Game.Enemies (moveEnemy)
import Game.Types

updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let enemySpeed = 50
  in map (moveEnemy (enemySpeed * dt)) enemiesList
