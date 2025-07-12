{-# LANGUAGE RecordWildCards #-}


-- Updated Game.Logic
module Game.Logic where


import Game.Enemies
import Game.Types
import Game.Config
import Data.List (mapAccumL, partition, find)
import Data.Maybe (listToMaybe)


cooldownFor :: TowerType -> Float
cooldownFor Archer = 0.5
cooldownFor Cannon = 1.0
cooldownFor Sniper = 2.0


towerDamageFor :: TowerType -> Int
towerDamageFor Archer = 20
towerDamageFor Cannon = 40
towerDamageFor Sniper = 80

towerRangeFor :: TowerType -> Float
towerRangeFor Archer = 150  -- radius in pixels
towerRangeFor Cannon = 100
towerRangeFor Sniper = 250

towerCooldownDuration :: Float
towerCooldownDuration = 1.0  -- 1 second

updateTowersCooldown :: Float -> [Tower] -> [Tower]
updateTowersCooldown dt = map updateTower
  where
    updateTower t = t { towerCooldown = max 0 (towerCooldown t - dt)
                      , towerTarget   = towerTarget t }


applyTowerDamage :: [Tower] -> [Enemy] -> ([Tower], [Enemy])
applyTowerDamage towers enemies = foldl attackIfReady ([], enemies) towers
  where
    attackIfReady :: ([Tower], [Enemy]) -> Tower -> ([Tower], [Enemy])
    attackIfReady (ts, es) tower
      | towerCooldown tower <= 0 =
          let (maybeEnemyHit, newEnemies) = attackOneEnemy tower es
              resetTower = tower { towerCooldown = cooldownFor (towerType tower)
                                 , towerTarget   = maybeEnemyHit
                                 }
          in (resetTower : ts, newEnemies)
      | otherwise =
          -- Check if target is still valid (alive & in range)
          let targetStillValid = case towerTarget tower of
                Just target ->
                  target `elem` es && inRange tower (enemyPosition target) (towerPos tower)
                Nothing -> False
              tower' = if targetStillValid then tower else tower { towerTarget = Nothing }
          in (tower' : ts, es)

    -- Each tower attacks only one enemy
    attackOneEnemy :: Tower -> [Enemy] -> (Maybe Enemy, [Enemy])
    attackOneEnemy tower es =
      let dmg = towerDamageFor (towerType tower)
          inRangeEnemies = filter (\e -> inRange tower (enemyPosition e) (towerPos tower)) es
          currentTarget = towerTarget tower

          -- Prioritize sticking to the previous target if valid
          chosenTarget =
            case currentTarget of
              Just t  -> if t `elem` inRangeEnemies then Just t else listToMaybe inRangeEnemies
              Nothing -> listToMaybe inRangeEnemies

      in case chosenTarget of
           Just target ->
             let updatedEnemies = map (damageIfTarget target dmg) es
                 updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
             in (updatedTarget, updatedEnemies)
           Nothing -> (Nothing, es)

    -- Apply damage only to selected target
    damageIfTarget :: Enemy -> Int -> Enemy -> Enemy
    damageIfTarget target dmg enemy
      | enemyPosition enemy == enemyPosition target = enemy { health = health enemy - dmg }
      | otherwise = enemy

    -- Check if enemy is in range of tower
    inRange :: Tower -> Position -> Position -> Bool
    inRange tower (x1, y1) (x2, y2) =
      sqrt ((x1 - x2)^2 + (y1 - y2)^2) <= towerRangeFor (towerType tower)


updateEnemies :: Float -> [Enemy] -> ([Enemy], [Enemy])
updateEnemies dt enemiesList =
  let movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      (reached, active) = partition (\(Enemy _ _ waypointIndex path _) ->
        waypointIndex >= length (pathWaypoints path)) movedEnemies
  in (active, reached)


updateWaveSystem :: Float -> GameState -> GameState
updateWaveSystem dt gs@GameState{..}
  -- 1. Wait for wave delay
  | wavePauseTimer > 0 =
      let newPause = max 0 (wavePauseTimer - dt)
          gs' = gs { wavePauseTimer = newPause }
      in if newPause == 0
         then
           let nextWave = currentWave + 1
           in if nextWave < length generateWaves
              then gs' { currentWave = nextWave
                       , waveQueue = generateWaves !! nextWave
                       , currentGroup = Nothing
                       , enemySpawnTimer = 0
                       , groupSpawnTimer = 0
                       }
              else gs'  -- No more waves
         else gs'

  -- 2. No current group, check if ready for next
  | otherwise = case currentGroup of

      Nothing ->
        if groupSpawnTimer > 0
        then gs { groupSpawnTimer = max 0 (groupSpawnTimer - dt) }
        else case waveQueue of
          [] ->
            gs { wavePauseTimer = waveDelay }
          (group:rest) ->
            gs { currentGroup = Just (group, 0)
               , waveQueue = rest
               , enemySpawnTimer = 0
               }

      -- 3. Currently spawning a group
      Just (enemiesInGroup, spawnedCount) ->
        let newEnemySpawnTimer = enemySpawnTimer - dt
        in if newEnemySpawnTimer <= 0
           then
             if spawnedCount < length enemiesInGroup
             then
               let enemyToSpawn = enemiesInGroup !! spawnedCount
                   newEnemiesList = enemies ++ [enemyToSpawn]
               in gs { enemies = newEnemiesList
                     , currentGroup = Just (enemiesInGroup, spawnedCount + 1)
                     , enemySpawnTimer = enemyDelay
                     }
             else
               gs { currentGroup = Nothing
                  , groupSpawnTimer = groupDelay
                  , enemySpawnTimer = 0
                  }
           else
             gs { enemySpawnTimer = newEnemySpawnTimer }


gainCoinsOnKills :: Int -> [Enemy] -> ([Enemy], Int)
gainCoinsOnKills coinReward enemies =
  let (alive, dead) = partition (\e -> health e > 0) enemies
  in (alive, length dead * coinReward)

