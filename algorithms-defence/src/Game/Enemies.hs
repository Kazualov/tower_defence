module Game.Enemies where

import System.Random (Random(randomR), StdGen, mkStdGen, split)  -- Add this import
import Graphics.Gloss
import Game.Types
import Game.Config



createEnemy :: EnemyType -> Game.Types.Path -> Enemy
createEnemy etype path =
  let spawnPoint = case path of
        Upper -> head upperPathWaypoints
        Lower -> head lowerPathWaypoints
  in Enemy etype spawnPoint 0 path (hpOf etype)

-- Custom drawing for boss
drawEnemy :: Enemy -> Picture
drawEnemy enemy@(Enemy Boss (x, y) _ _ hp) =
  translate x y $
    Pictures
      [ 
      translate (-20) (-8) $ scale 0.2 0.2 $ color black $ Text "Nikolay Kudasov"
      , translate (-15) 15 $ scale 0.1 0.1 $ color red $ text (show hp)
      ]

drawEnemy enemy@(Enemy etype (x, y) _ _ hp) = 
  translate x y $
    Pictures
      [ color (makeColorI 255 255 255 255) $
          thickCircle 8 1
      , translate (-textOffset etype) (-6) $
          scale 0.15 0.15 $ color black $ Text (enemyLabel etype)
      , translate (-10) 12 $
          scale 0.1 0.1 $ color red $ text (show hp)
      ]

enemyLabel :: EnemyType -> String
enemyLabel (EChar c)      = [c]
enemyLabel (EInt n)       = show n
enemyLabel (EString s)    = s
enemyLabel (Boss)         = "Nikolay Kudasov"
enemyLabel (EList xs)     = "[" ++ joinWith ", " (map enemyLabel xs) ++ "]"
enemyLabel (EMap kvs)     = "{" ++ joinWith ", " (map showKV kvs) ++ "}"
  where
    showKV (k, v) = k ++ ": " ++ enemyLabel v

joinWith :: String -> [String] -> String
joinWith _   []     = ""
joinWith _   [x]    = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

textOffset :: EnemyType -> Float
textOffset etype = fromIntegral (length (enemyLabel etype)) * 3

pathWaypoints :: Game.Types.Path -> [Position]
pathWaypoints Upper = upperPathWaypoints
pathWaypoints Lower = lowerPathWaypoints

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt enemy@(Enemy etype pos@(x,y) wpIndex path hp)
  | wpIndex >= length waypoints = enemy
  | otherwise = 
      let (currentX, currentY) = waypoints !! wpIndex
          nextIndex = min (wpIndex + 1) (length waypoints - 1)
          (nextX, nextY) = waypoints !! nextIndex
          dx = nextX - currentX
          dy = nextY - currentY
          distance = sqrt (dx * dx + dy * dy)
          speed = if isBoss enemy then bossSpeed else enemySpeed
          moveDist = speed * dt
          ratio = moveDist / max 1 distance
          newX = x + dx * ratio
          newY = y + dy * ratio
          distToNext = sqrt ((nextX - newX)^2 + (nextY - newY)^2)
          newWpIndex = if distToNext < 5 then nextIndex else wpIndex
      in Enemy etype (newX, newY) newWpIndex path hp
  where
    waypoints = pathWaypoints path

createBoss :: Game.Types.Path -> Enemy
createBoss path = Enemy Boss startPos 0 path bossHealth
  where
    startPos = case path of
      Upper -> head upperPathWaypoints
      Lower -> head lowerPathWaypoints

isBoss :: Enemy -> Bool
isBoss (Enemy Boss _ _ _ _) = True
isBoss _ = False

spawnBossChildren :: Enemy -> [Enemy]
spawnBossChildren (Enemy _ (x,y) wpIndex path _) =
  let child1 = Enemy (EString "Nikolay") (x - bossChildOffset, y) wpIndex path bossChildHealth
      child2 = Enemy (EString "Kudasov") (x + bossChildOffset, y) wpIndex path bossChildHealth
  in [child1, child2]

generateWaves :: [[ [Enemy] ]]
generateWaves =
  [ [ [ createEnemy (EChar 'D') Upper | _ <- [1..4] ]
    , [ createEnemy (EInt 3) Lower | _ <- [1..4] ]
    , [ createEnemy (EString "c") Upper | _ <- [1..4] ]
    , [ createEnemy (EChar 'E') Lower | _ <- [1..3] ]
    , [ createEnemy (EString "d") Upper | _ <- [1..4] ]
    , [ createEnemy (EList [EInt 1, EInt 2, EInt 3]) Upper ]
    , [ createEnemy (EMap [("key1", EInt 42), ("key2", EChar 'x')]) Lower ]
  ]
  , [ [ createEnemy (EChar 'B') Lower | _ <- [1..4] ]
    , [ createEnemy (EInt 2) Upper | _ <- [1..3] ]
    , [ createEnemy (EString "b") Lower | _ <- [1..4] ]
    , [ createEnemy (EChar 'C') Upper | _ <- [1..3] ]
    ]
  ,
    [ [ createEnemy (EChar 'A') Upper | _ <- [1..3] ]
    , [ createEnemy (EInt 1) Lower | _ <- [1..5] ]
    , [ createEnemy (EString "hello") Upper | _ <- [1..3] ]
    ]
  , [ [ createBoss Upper ] ]
  ]


-- Generate random waves
generateRandomWaves :: StdGen -> Int -> ([[[Enemy]]], StdGen)
generateRandomWaves gen waveCount =
  generateWavesRec gen waveCount []
  where
    generateWavesRec :: StdGen -> Int -> [[[Enemy]]] -> ([[[Enemy]]], StdGen)
    generateWavesRec g 0 acc = (reverse acc, g)
    generateWavesRec g n acc =
      let (wave, newGen) = generateRandomWave (createWaveParams n difficultyMultiplier) g
      in generateWavesRec newGen (n-1) (wave:acc)

    -- Add difficulty setting
    createWaveParams :: Int -> Float -> WaveParams
    createWaveParams waveNum difficultyMultiplier = WaveParams
      { waveSize = 5 + waveNum * 2
      , groupCount = 1 + (waveNum `div` 3)
      , intensity = min 2.0 (0.2 + fromIntegral waveNum * 0.1 * difficultyMultiplier)
      }


-- Generate a single random wave
generateRandomWave :: WaveParams -> StdGen -> ([[Enemy]], StdGen)
generateRandomWave params gen =
  let (enemiesPerGroup, gen1) = distributeEnemies (waveSize params) (groupCount params) gen
      (groups, finalGen) = generateGroupsRec enemiesPerGroup params gen1 []
  in (groups, finalGen)

-- Distribute enemies into groups
distributeEnemies :: Int -> Int -> StdGen -> ([Int], StdGen)
distributeEnemies total groups gen =
  distributeRec gen groups total []
  where
    distributeRec :: StdGen -> Int -> Int -> [Int] -> ([Int], StdGen)
    distributeRec g 1 remaining acc = (reverse (remaining:acc), g)
    distributeRec g n remaining acc =
      let (count, newGen) = randomR (1, remaining - (n - 1)) g
      in distributeRec newGen (n-1) (remaining - count) (count:acc)

-- Generate enemy groups recursively
generateGroupsRec :: [Int] -> WaveParams -> StdGen -> [[Enemy]] -> ([[Enemy]], StdGen)
generateGroupsRec [] _ g acc = (reverse acc, g)
generateGroupsRec (size:sizes) params g acc =
  let (pathRand, g1) = randomR (0.0 :: Float, 1.0) g
      path = if pathRand < 0.5 then Upper else Lower
      (group, newGen) = generateEnemyGroup size (intensity params) path g1
  in generateGroupsRec sizes params newGen (group:acc)

-- Generate a group of enemies
generateEnemyGroup :: Int -> Float -> Game.Types.Path -> StdGen -> ([Enemy], StdGen)
generateEnemyGroup size intensity path gen =
  generateEnemiesRec size gen []
  where
    generateEnemiesRec :: Int -> StdGen -> [Enemy] -> ([Enemy], StdGen)
    generateEnemiesRec 0 g acc = (reverse acc, g)
    generateEnemiesRec n g acc =
      let (typeRand, g1) = randomR (0.0 :: Float, 1.0) g
          enemyType = selectEnemyType typeRand intensity
          g2 = g1  -- not used anymore but required for chaining
          baseHealth = hpOf enemyType
          enemy = (createEnemy enemyType path) { health = baseHealth }
      in generateEnemiesRec (n-1) g2 (enemy:acc)


-- enemyType = selectEnemyType typeRand intensity
          -- (healthRand, g2) = randomR (0.8, 1.2) g1
          -- baseHealth = hpOf enemyType
          -- adjustedHealth = round (fromIntegral baseHealth * healthRand * (1 + intensity))
          -- enemy = (createEnemy enemyType path) { health = adjustedHealth }

-- Select enemy type based on random value and intensity
selectEnemyType :: Float -> Float -> EnemyType
selectEnemyType r intensity
  | r < 0.2 * intensity    = EList [EInt 1, EInt 2, EInt 3]
  | r < 0.3 * intensity    = EMap [("key", EInt 42)]
  | r < 0.4 + 0.1 * intensity = EString "hard"
  | r < 0.5 + 0.1 * intensity = EInt 52
  | otherwise              = EChar 'A'
