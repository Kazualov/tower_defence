{-# LANGUAGE RecordWildCards #-}

module Game.Logic where

import Game.Enemies
import Game.Types
import Game.Config
import Data.List (partition, find)
import Data.Maybe (listToMaybe)

-- | Decrease the cooldown for each tower by the elapsed time
updateTowersCooldown :: Float -> [Tower] -> [Tower]
updateTowersCooldown dt = map updateTower
  where
    updateTower t = t 
      { towerCooldown = max 0 (towerCooldown t - dt)
      , towerTarget   = towerTarget t
      }

-- | Handle all tower attacks, applying damage and resetting cooldowns
applyTowerDamage :: [Tower] -> [Enemy] -> ([Tower], [Enemy])
applyTowerDamage [] enemies = ([], enemies)
applyTowerDamage (tower:rest) enemies =
  let (updatedTowers, currentEnemies) = applyTowerDamage rest enemies
  in case () of
    _ 
      -- Tower is ready to attack
      | towerCooldown tower <= 0 ->
          let (maybeEnemyHit, newEnemies) = attackOneEnemy tower currentEnemies
              resetTower = tower
                { towerCooldown = cooldownFor (towerType tower)
                , towerTarget   = maybeEnemyHit
                }
          in (resetTower : updatedTowers, newEnemies)

      -- Tower is not ready, but target is still valid
      | targetStillValid ->
          (tower : updatedTowers, currentEnemies)

      -- Tower is not ready and target is invalid
      | otherwise ->
          (tower { towerTarget = Nothing } : updatedTowers, currentEnemies)
  where
    targetStillValid = case towerTarget tower of
      Just target -> target `elem` enemies &&
                     inRange tower (enemyPosition target) (towerPos tower)
      Nothing -> False


    -- Attack a single enemy (if one is in range)
    attackOneEnemy :: Tower -> [Enemy] -> (Maybe Enemy, [Enemy])
    attackOneEnemy tower es =
      let inRangeEnemies = filter (\e -> inRange tower (enemyPosition e) (towerPos tower)) es
          chosenTarget = case towerTarget tower of
            Just t  -> if t `elem` inRangeEnemies then Just t else listToMaybe inRangeEnemies
            Nothing -> listToMaybe inRangeEnemies
      in case chosenTarget of
           Just target -> applyModificatorAttack tower target es
           Nothing     -> (Nothing, es)

    -- Apply the appropriate modificator or damage to the target (and nearby enemies)
    applyModificatorAttack :: Tower -> Enemy -> [Enemy] -> (Maybe Enemy, [Enemy])
    applyModificatorAttack tower target es =
      case towerMod tower of
        Just Pop ->
          let updatedEnemies = map (\e -> if enemyPosition e == enemyPosition target 
                                          then e { health = health e - 99999 } else e) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)

        Just Map ->
          let dmg = towerDamageForMap (towerType tower)
              updatedEnemies = map (\e -> if distance (enemyPosition target) (enemyPosition e) <= mapModRadius
                                          then e { health = health e - dmg } else e) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)

        -- No modificator: apply direct or splash damage
        _ ->
          let dmg = towerDamageFor (towerType tower)
              updatedEnemies = map (damageIfTarget tower target dmg) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)

    -- Apply damage based on tower type
    damageIfTarget :: Tower -> Enemy -> Int -> Enemy -> Enemy
    damageIfTarget tower target dmg enemy
      | towerType tower == Cannon &&
        distance (enemyPosition target) (enemyPosition enemy) <= blastRadius =
          enemy { health = health enemy - dmg }
      | enemyPosition enemy == enemyPosition target =
          enemy { health = health enemy - dmg }
      | otherwise = enemy

    -- Check if an enemy is within tower range
    inRange :: Tower -> Position -> Position -> Bool
    inRange tower (x1, y1) (x2, y2) =
      sqrt ((x1 - x2)^2 + (y1 - y2)^2) <= towerRangeFor (towerType tower)

-- | Assign a modificator to a tower
applyModificator :: Modificator -> Tower -> Tower
applyModificator mod tower = tower { towerMod = Just mod }

-- | Check if a modificator can be applied to a tower
canApplyModificator :: Modificator -> Tower -> Bool
canApplyModificator GarbageCollector _ = True
canApplyModificator _ tower = towerMod tower == Nothing

-- | Find a tower near a given position (for selection)
findTowerAt :: Position -> [Tower] -> Maybe Tower
findTowerAt (x, y) towers = find (\t -> distance (towerPos t) (x, y) <= 25) towers

-- | Move all enemies; return (active, reached-end) enemies
updateEnemies :: Float -> [Enemy] -> ([Enemy], [Enemy])
updateEnemies dt enemiesList =
  let movedEnemies = map (moveEnemy dt) enemiesList
      (reached, active) = partition (\(Enemy _ _ waypointIndex path _) ->
        waypointIndex >= length (pathWaypoints path)) movedEnemies
  in (active, reached)

-- | Compute Euclidean distance between two points
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- | Total damage to tower from all enemies that reached it
updateEnemyDamage :: [Enemy] -> Int
updateEnemyDamage enemies =
  sum $ map (\e -> if isBoss e then bossDamage else 40) enemies

-- | Process enemy deaths; return (survivors, coins gained, boss spawns)
gainCoinsOnKillsWithBoss :: Int -> [Enemy] -> ([Enemy], Int, [Enemy])
gainCoinsOnKillsWithBoss coinsPerKill enemies =
  let (dead, alive) = partition (\e -> health e <= 0) enemies
      bossChildren = concatMap (\e -> if isBoss e then spawnBossChildren e else []) dead
      coinGain = length dead * coinsPerKill
  in (alive, coinGain, bossChildren)

-- | If current group is done or not ready, update group spawn timer
handleGroupDelay :: Float -> GameState -> GameState
handleGroupDelay dt gs@GameState{..}
  | groupSpawnTimer > 0 =
      gs { groupSpawnTimer = max 0 (groupSpawnTimer - dt) }
  | otherwise = case waveQueue of
      [] -> gs { wavePauseTimer = waveDelay }
      (wave:rest) -> case wave of
        [] -> gs { waveQueue = rest, wavePauseTimer = waveDelay }
        (group:groups) -> gs 
          { currentGroup = Just (group, 0)
          , waveQueue = groups : rest
          , enemySpawnTimer = 0
          }

-- | Spawn enemies one by one from a group
spawnEnemiesFromGroup :: Float -> GameState -> [Enemy] -> Int -> GameState
spawnEnemiesFromGroup dt gs@GameState{..} enemiesInGroup spawnedCount
  | newEnemySpawnTimer > 0 = gs { enemySpawnTimer = newEnemySpawnTimer }
  | spawnedCount < length enemiesInGroup =
      let enemyToSpawn = enemiesInGroup !! spawnedCount
          newEnemiesList = enemies ++ [enemyToSpawn]
          newSpawnTimer = if isBoss enemyToSpawn then bossChildDelay else enemyDelay
      in gs 
        { enemies = newEnemiesList
        , currentGroup = Just (enemiesInGroup, spawnedCount + 1)
        , enemySpawnTimer = newSpawnTimer
        }
  | otherwise = gs 
      { currentGroup = Nothing
      , groupSpawnTimer = groupDelay
      , enemySpawnTimer = 0
      }
  where
    newEnemySpawnTimer = enemySpawnTimer - dt

-- | Main function to handle wave progression and group spawning
updateWaveSystem :: Float -> GameState -> GameState
updateWaveSystem dt gs@GameState{..}
  | wavePauseTimer > 0 = handleWavePause dt gs
  | otherwise = case currentGroup of
      Nothing -> handleGroupDelay dt gs
      Just (group, count) -> spawnEnemiesFromGroup dt gs group count

-- | Reduce wave pause timer or start next wave
handleWavePause :: Float -> GameState -> GameState
handleWavePause dt gs@GameState{..} =
  let newPause = max 0 (wavePauseTimer - dt)
      gs' = gs { wavePauseTimer = newPause }
  in if newPause == 0 then startNextWave gs' else gs'

-- | Start the next wave or boss depending on wave index
startNextWave :: GameState -> GameState
startNextWave gs@GameState{..}
  | currentWave < 8 =
      let nextWave = currentWave + 1
          (newWave, newGen) = generateRandomWave (createWaveParams nextWave) randomGen
      in gs 
          { currentWave = nextWave
          , waveQueue = [newWave] ++ waveQueue
          , currentGroup = Nothing
          , enemySpawnTimer = 0
          , groupSpawnTimer = 0
          , randomGen = newGen
          }
  | currentWave == 8 =
      gs { currentWave = 9
         , waveQueue = [[[createBoss Upper]]]
         , currentGroup = Nothing
         , enemySpawnTimer = 0
         , groupSpawnTimer = 0
         }
  | otherwise = gs
