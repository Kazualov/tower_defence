-- Updated Game.Logic
module Game.Logic where

import Game.Enemies (moveEnemy)
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
        go enemy@(Enemy t (ex, ey) w h) (acc, attacked)
          | not attacked && inRange (ex, ey) (tx, ty) =
              (enemy { health = h - dmg } : acc, True)
          | otherwise = (enemy : acc, attacked)

    inRange :: Position -> Position -> Bool
    inRange (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2) <= towerRange




updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies dt enemiesList =
  let enemySpeed = 50
      movedEnemies = map (moveEnemy (enemySpeed * dt)) enemiesList
      -- Filter out enemies that have reached the end
      activeEnemies = filter (\(Enemy _ _ waypointIndex _) -> waypointIndex < length pathWaypoints) movedEnemies
  in activeEnemies

  