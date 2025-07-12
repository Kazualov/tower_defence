{-# LANGUAGE RecordWildCards #-}


-- Updated Game.Logic
module Game.Logic where

import Game.Enemies
import Game.Types
import Game.Config
import Data.List (mapAccumL)

cooldownFor :: TowerType -> Float
cooldownFor Archer = 0.5
cooldownFor Cannon = 1.0
cooldownFor Sniper = 2.0


towerDamageFor :: TowerType -> Int
towerDamageFor Archer = 20
towerDamageFor Cannon = 40
towerDamageFor Sniper = 80

towerRange :: Float
towerRange = 100  -- radius in pixels

towerCooldownDuration :: Float
towerCooldownDuration = 1.0  -- 1 second

updateTowersCooldown :: Float -> [Tower] -> [Tower]
updateTowersCooldown dt = map updateTower
  where
    updateTower t = t { towerCooldown = max 0 (towerCooldown t - dt) }


applyTowerDamage :: [Tower] -> [Enemy] -> ([Tower], [Enemy])
applyTowerDamage towers enemies = foldl attackIfReady ([], enemies) towers
  where
    attackIfReady :: ([Tower], [Enemy]) -> Tower -> ([Tower], [Enemy])
    attackIfReady (ts, es) tower
      | towerCooldown tower <= 0 =
          let (damagedEnemies, attacked) = damageEnemiesInRange tower es
              resetTower = tower { towerCooldown = cooldownFor (towerType tower) }
          in (resetTower : ts, damagedEnemies)
      | otherwise = (tower : ts, es)

    damageEnemiesInRange :: Tower -> [Enemy] -> ([Enemy], Bool)
    damageEnemiesInRange tower = foldr go ([], False)
      where
        (tx, ty) = towerPos tower
        dmg = towerDamageFor (towerType tower)
        go enemy@(Enemy t (ex, ey) _ w h) (acc, attacked)
          | not attacked && inRange (ex, ey) (tx, ty) =
              (enemy { health = h - dmg } : acc, True)
          | otherwise = (enemy : acc, attacked)

    inRange :: Position -> Position -> Bool
    inRange (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2) <= towerRange


updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      activeEnemies = filter (\(Enemy _ _ waypointIndex path _) ->
        waypointIndex < length (pathWaypoints path)) movedEnemies
  in activeEnemies

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
