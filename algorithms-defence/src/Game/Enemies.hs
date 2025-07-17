module Game.Enemies where

import System.Random (Random(randomR), StdGen)
import Graphics.Gloss
import Game.Types
import Game.Config
import Debug.Trace (trace)

-- Create an enemy at the starting position based on its path (Upper/Lower)
createEnemy :: EnemyType -> Game.Types.Path -> Enemy
createEnemy etype path =
  let spawnPoint = case path of
        Upper -> head upperPathWaypoints
        Lower -> head lowerPathWaypoints
  in Enemy etype spawnPoint 0 path (hpOf etype)

-- Draw a boss enemy with a custom label and health indicator
drawEnemy :: Enemy -> Picture
drawEnemy enemy@(Enemy Boss (x, y) _ _ hp) =
  translate x y $
    Pictures
      [ translate (-20) (-8) $ scale 0.2 0.2 $ color black $ Text "Nikolay Kudasov"
      , translate (-15) 15 $ scale 0.1 0.1 $ color red $ text (show hp)
      ]

-- Draw a regular enemy with label and health
drawEnemy enemy@(Enemy etype (x, y) _ _ hp) = 
  translate x y $
    Pictures
      [ color (makeColorI 255 255 255 255) $ thickCircle 8 1
      , translate (-textOffset etype) (-6) $ scale 0.15 0.15 $ color black $ Text (enemyLabel etype)
      , translate (-10) 12 $ scale 0.1 0.1 $ color red $ text (show hp)
      ]

-- Get a display label for each enemy type
enemyLabel :: EnemyType -> String
enemyLabel (EChar c)   = [c]
enemyLabel (EInt n)    = show n
enemyLabel (EString s) = s
enemyLabel (Boss)      = "Nikolay Kudasov"
enemyLabel (EList xs)  = "[" ++ joinWith ", " (map enemyLabel xs) ++ "]"
enemyLabel (EMap kvs)  = "{" ++ joinWith ", " (map showKV kvs) ++ "}"
  where showKV (k, v) = k ++ ": " ++ enemyLabel v

-- Join list of strings with a separator
joinWith :: String -> [String] -> String
joinWith _   []     = ""
joinWith _   [x]    = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

-- Offset text based on its length to center it
textOffset :: EnemyType -> Float
textOffset etype = fromIntegral (length (enemyLabel etype)) * 3

-- Get the list of waypoints based on path
pathWaypoints :: Game.Types.Path -> [Position]
pathWaypoints Upper = upperPathWaypoints
pathWaypoints Lower = lowerPathWaypoints

-- Move an enemy along its path based on elapsed time
moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt enemy@(Enemy etype (x, y) wpIndex path hp)
  | wpIndex >= length waypoints = enemy  -- Out of path
  | otherwise =
      let (tx, ty) = waypoints !! wpIndex
          dx = tx - x
          dy = ty - y
          dist = sqrt (dx * dx + dy * dy)
          speed = if isBoss enemy then bossSpeed else enemySpeed
          moveDist = min dist (speed * dt)
          ratio = if dist == 0 then 0 else moveDist / dist
          newX = x + dx * ratio
          newY = y + dy * ratio
          distAfterMove = sqrt ((tx - newX)^2 + (ty - newY)^2)
          reached = distAfterMove < 1.0
          newWpIndex = if reached then wpIndex + 1 else wpIndex
      in Enemy etype (newX, newY) newWpIndex path hp
  where
    waypoints = pathWaypoints path

-- Create the boss enemy
createBoss :: Game.Types.Path -> Enemy
createBoss path = Enemy Boss startPos 0 path bossHealth
  where
    startPos = case path of
      Upper -> head upperPathWaypoints
      Lower -> head lowerPathWaypoints

-- Check if an enemy is the boss
isBoss :: Enemy -> Bool
isBoss (Enemy Boss _ _ _ _) = True
isBoss _ = False

-- Split the boss into two smaller enemies upon defeat
spawnBossChildren :: Enemy -> [Enemy]
spawnBossChildren (Enemy _ (x,y) wpIndex path _) =
  let child1 = Enemy (EString "Nikolay") (x - bossChildOffset, y) wpIndex path bossChildHealth
      child2 = Enemy (EString "Kudasov") (x + bossChildOffset, y) wpIndex path bossChildHealth
  in [child1, child2]

-- Generate a full list of waves with groups of enemies
generateRandomWaves :: StdGen -> Int -> ([[[Enemy]]], StdGen)
generateRandomWaves gen waveCount =
  let (waves, newGen) = generateWavesRec gen waveCount []
      bossWave = [[createBoss Upper]]  -- Final wave has the boss
  in (waves ++ [bossWave], newGen)
  where
    generateWavesRec :: StdGen -> Int -> [[[Enemy]]] -> ([[[Enemy]]], StdGen)
    generateWavesRec g 0 acc = (reverse acc, g)
    generateWavesRec g n acc =
      let (wave, newGen) = generateRandomWave (createWaveParams n) g
      in generateWavesRec newGen (n-1) (wave:acc)

-- Generate a single wave made of groups
generateRandomWave :: WaveParams -> StdGen -> ([[Enemy]], StdGen)
generateRandomWave params gen =
  let (enemiesPerGroup, gen1) = distributeEnemies (waveSize params) (groupCount params) gen
      (groups, finalGen) = generateGroupsRec enemiesPerGroup params gen1 []
  in (groups, finalGen)

-- Distribute total enemies into random-sized groups
distributeEnemies :: Int -> Int -> StdGen -> ([Int], StdGen)
distributeEnemies total groups gen = distributeRec gen groups total []
  where
    distributeRec :: StdGen -> Int -> Int -> [Int] -> ([Int], StdGen)
    distributeRec g 1 remaining acc = (reverse (remaining:acc), g)
    distributeRec g n remaining acc =
      let (count, newGen) = randomR (1, remaining - (n - 1)) g
      in distributeRec newGen (n-1) (remaining - count) (count:acc)

-- Generate groups recursively from sizes
generateGroupsRec :: [Int] -> WaveParams -> StdGen -> [[Enemy]] -> ([[Enemy]], StdGen)
generateGroupsRec [] _ g acc = (reverse acc, g)
generateGroupsRec (size:sizes) params g acc =
  let (pathRand, g1) = randomR (0.0 :: Float, 1.0) g
      path = if pathRand < 0.5 then Upper else Lower
      (group, newGen) = generateEnemyGroup size (intensity params) path g1
  in generateGroupsRec sizes params newGen (group:acc)

-- Generate a group of enemies with random types
generateEnemyGroup :: Int -> Float -> Game.Types.Path -> StdGen -> ([Enemy], StdGen)
generateEnemyGroup size intensity path gen =
  generateEnemiesRec size gen []
  where
    generateEnemiesRec :: Int -> StdGen -> [Enemy] -> ([Enemy], StdGen)
    generateEnemiesRec 0 g acc = (reverse acc, g)
    generateEnemiesRec n g acc =
      let (typeRand, g1) = randomR (0.0 :: Float, 1.0) g
          enemyType = selectEnemyType typeRand intensity
          enemy = createEnemy enemyType path
      in generateEnemiesRec (n-1) g1 (enemy:acc)

-- Define difficulty scaling for each wave
createWaveParams :: Int -> WaveParams
createWaveParams waveNum = WaveParams
  { waveSize = 5 + waveNum * 2
  , groupCount = 1 + (waveNum `div` 3)
  , intensity = min 1.0 (0.2 + fromIntegral waveNum * 0.1 * difficultyMultiplier)
  }

-- Randomly choose enemy type based on wave intensity
selectEnemyType :: Float -> Float -> EnemyType
selectEnemyType r intensity
  | r < 0.2 * intensity        = EList [EInt 1, EInt 2, EInt 3]
  | r < 0.3 * intensity        = EMap [("key", EInt 42)]
  | r < 0.4 + 0.1 * intensity  = EString "hard"
  | r < 0.5 + 0.1 * intensity  = EInt 52
  | otherwise                  = EChar 'A'
