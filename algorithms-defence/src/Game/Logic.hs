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
          let targetStillValid = case towerTarget tower of
                Just target ->
                  target `elem` es && inRange tower (enemyPosition target) (towerPos tower)
                Nothing -> False
              tower' = if targetStillValid then tower else tower { towerTarget = Nothing }
          in (tower' : ts, es)

    attackOneEnemy :: Tower -> [Enemy] -> (Maybe Enemy, [Enemy])
    attackOneEnemy tower es =
      let inRangeEnemies = filter (\e -> inRange tower (enemyPosition e) (towerPos tower)) es
          currentTarget = towerTarget tower
          chosenTarget =
            case currentTarget of
              Just t  -> if t `elem` inRangeEnemies then Just t else listToMaybe inRangeEnemies
              Nothing -> listToMaybe inRangeEnemies
      in case chosenTarget of
           Just target -> applyModificatorAttack tower target es
           Nothing -> (Nothing, es)

    applyModificatorAttack :: Tower -> Enemy -> [Enemy] -> (Maybe Enemy, [Enemy])
    applyModificatorAttack tower target es =
      case towerMod tower of
        Just Pop -> 
          let updatedEnemies = map (\e -> if enemyPosition e == enemyPosition target 
                                         then e { health = health e - 99999 }
                                         else e) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)
        
        Just Map -> 
          let dmg = towerDamageForMap (towerType tower)
              updatedEnemies = map (\e -> if distance (enemyPosition target) (enemyPosition e) <= mapModRadius
                                         then e { health = health e - dmg }
                                         else e) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)
        
        _ -> 
          let dmg = towerDamageFor (towerType tower)
              updatedEnemies = map (damageIfTarget tower target dmg) es
              updatedTarget = find (\e -> enemyPosition e == enemyPosition target) updatedEnemies
          in (updatedTarget, updatedEnemies)

    damageIfTarget :: Tower -> Enemy -> Int -> Enemy -> Enemy
    damageIfTarget tower target dmg enemy
      | towerType tower == Cannon =
          if distance (enemyPosition target) (enemyPosition enemy) <= blastRadius
          then enemy { health = health enemy - dmg }
          else enemy
      | enemyPosition enemy == enemyPosition target =
          enemy { health = health enemy - dmg }
      | otherwise = enemy

    inRange :: Tower -> Position -> Position -> Bool
    inRange tower (x1, y1) (x2, y2) =
      sqrt ((x1 - x2)^2 + (y1 - y2)^2) <= towerRangeFor (towerType tower)

applyModificator :: Modificator -> Tower -> Tower
applyModificator mod tower = tower { towerMod = Just mod }

canApplyModificator :: Modificator -> Tower -> Bool
canApplyModificator GarbageCollector _ = True
canApplyModificator _ tower = towerMod tower == Nothing

findTowerAt :: Position -> [Tower] -> Maybe Tower
findTowerAt (x, y) towers = find (\t -> distance (towerPos t) (x, y) <= 25) towers

updateEnemies :: Float -> [Enemy] -> ([Enemy], [Enemy])
updateEnemies dt enemiesList =
  let movedEnemies = map (moveEnemy dt) enemiesList
      (reached, active) = partition (\(Enemy _ _ waypointIndex path _) ->
        waypointIndex >= length (pathWaypoints path)) movedEnemies
  in (active, reached)

distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

updateEnemyDamage :: [Enemy] -> Int
updateEnemyDamage enemies =
  sum $ map (\e -> if isBoss e then bossDamage else 40) enemies

gainCoinsOnKillsWithBoss :: Int -> [Enemy] -> ([Enemy], Int, [Enemy])
gainCoinsOnKillsWithBoss coinsPerKill enemies =
  let (dead, alive) = partition (\e -> health e <= 0) enemies
      bossChildren = concatMap (\e -> if isBoss e && health e <= 0 then spawnBossChildren e else []) dead
      coinGain = length dead * coinsPerKill
  in (alive, coinGain, bossChildren)

handleGroupDelay :: Float -> GameState -> GameState
handleGroupDelay dt gs@GameState{..}
  | groupSpawnTimer > 0 =
      gs { groupSpawnTimer = max 0 (groupSpawnTimer - dt) }
  | otherwise = case waveQueue of
      [] -> gs { wavePauseTimer = waveDelay }
      (wave:rest) -> case wave of
        [] -> gs { waveQueue = rest, wavePauseTimer = waveDelay }
        (group:groups) -> gs { currentGroup = Just (group, 0)
                             , waveQueue = groups : rest
                             , enemySpawnTimer = 0
                             }

spawnEnemiesFromGroup :: Float -> GameState -> [Enemy] -> Int -> GameState
spawnEnemiesFromGroup dt gs@GameState{..} enemiesInGroup spawnedCount
  | newEnemySpawnTimer > 0 = gs { enemySpawnTimer = newEnemySpawnTimer }
  | spawnedCount < length enemiesInGroup =
      let enemyToSpawn = enemiesInGroup !! spawnedCount
          newEnemiesList = enemies ++ [enemyToSpawn]
          newSpawnTimer = if isBoss enemyToSpawn then bossChildDelay else enemyDelay
      in gs { enemies = newEnemiesList
            , currentGroup = Just (enemiesInGroup, spawnedCount + 1)
            , enemySpawnTimer = newSpawnTimer
            }
  | otherwise = gs { currentGroup = Nothing
                   , groupSpawnTimer = groupDelay
                   , enemySpawnTimer = 0
                   }
  where
    newEnemySpawnTimer = enemySpawnTimer - dt

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
startNextWave gs@GameState{..}
  | currentWave < 8 =  -- 8 for 9 random waves (0-based index)
      let nextWave = currentWave + 1
          (newWave, newGen) = generateRandomWave (createWaveParams nextWave) randomGen
      in gs { currentWave = nextWave
            , waveQueue = [newWave] ++ waveQueue
            , currentGroup = Nothing
            , enemySpawnTimer = 0
            , groupSpawnTimer = 0
            , randomGen = newGen
            }
  | currentWave == 8 =  -- Switch to boss wave
      gs { currentWave = 9
         , waveQueue = [[[createBoss Upper]]]
         , currentGroup = Nothing
         , enemySpawnTimer = 0
         , groupSpawnTimer = 0
         }
  | otherwise = gs  -- No more waves after boss