{-# LANGUAGE RecordWildCards #-}

module Game.Logic where

import Game.Enemies
import Game.Types
import Game.Config
import Data.List (partition, find)
import Data.Maybe (listToMaybe)

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
             let updatedEnemies = map (damageIfTarget tower target dmg) es
                 updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
             in (updatedTarget, updatedEnemies)
           Nothing -> (Nothing, es)

    -- Apply damage only to selected target
    damageIfTarget :: Tower -> Enemy -> Int -> Enemy -> Enemy
    damageIfTarget tower target dmg enemy
      | towerType tower == Cannon =
          if distance (enemyPosition target) (enemyPosition enemy) <= blastRadius
          then enemy { health = health enemy - dmg }
          else enemy
      | enemyPosition enemy == enemyPosition target =
          enemy { health = health enemy - dmg }
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

distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


updateWaveSystem :: Float -> GameState -> GameState
updateWaveSystem dt gs@GameState{..}
  | wavePauseTimer > 0 = handleWavePause dt gs
  | otherwise = case currentGroup of
      Nothing -> handleGroupDelay dt gs
      Just (group, count) -> spawnEnemiesFromGroup dt gs group count

handleWavePause :: Float -> GameState -> GameState
handleWavePause dt gs@GameState{..} =
  let newPause = max 0 (wavePauseTimer - dt)
      gs' = gs { wavePauseTimer = newPause }
  in if newPause == 0
     then startNextWave gs'
     else gs'

startNextWave :: GameState -> GameState
startNextWave gs@GameState{..} =
  let (newWave, newGen) = generateRandomWave (createWaveParams (currentWave + 1)) randomGen
  in gs { waveQueue = waveQueue ++ [newWave]
        , randomGen = newGen
        , currentWave = currentWave + 1
        }
  where
    createWaveParams n = WaveParams
      { waveSize = 5 + n * 2
      , groupCount = 1 + n `div` 3
      , intensity = min 1.0 (0.2 + fromIntegral n * 0.1)
      }


handleGroupDelay :: Float -> GameState -> GameState
handleGroupDelay dt gs@GameState{..}
  | groupSpawnTimer > 0 =
      gs { groupSpawnTimer = max 0 (groupSpawnTimer - dt) }
  | otherwise = case waveQueue of
      [] -> gs { wavePauseTimer = waveDelay }
      (wave:restWaves) -> case wave of
        [] -> gs { waveQueue = restWaves }  -- skip empty wave
        (group:restGroups) -> gs { currentGroup = Just (group, 0)
                                 , waveQueue = restGroups : restWaves
                                 , enemySpawnTimer = 0
                                 }



spawnEnemiesFromGroup :: Float -> GameState -> [Enemy] -> Int -> GameState
spawnEnemiesFromGroup dt gs@GameState{..} enemiesInGroup spawnedCount
  | newEnemySpawnTimer > 0 = gs { enemySpawnTimer = newEnemySpawnTimer }

  | spawnedCount < length enemiesInGroup =
      let enemyToSpawn   = enemiesInGroup !! spawnedCount
          newEnemiesList = enemies ++ [enemyToSpawn]
      in gs { enemies        = newEnemiesList
            , currentGroup   = Just (enemiesInGroup, spawnedCount + 1)
            , enemySpawnTimer = enemyDelay
            }

  | otherwise = gs { currentGroup    = Nothing
                   , groupSpawnTimer = groupDelay
                   , enemySpawnTimer = 0
                   }
  where
    newEnemySpawnTimer = enemySpawnTimer - dt


gainCoinsOnKills :: Int -> [Enemy] -> ([Enemy], Int)
gainCoinsOnKills coinReward enemies =
  let (alive, dead) = partition (\e -> health e > 0) enemies
  in (alive, length dead * coinReward)

