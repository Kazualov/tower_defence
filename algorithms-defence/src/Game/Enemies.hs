module Game.Enemies where

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
  [ -- Wave 1
  --   [ [ createEnemy (EChar 'D') Upper | _ <- [1..4] ]
  --   , [ createEnemy (EInt 3) Lower | _ <- [1..4] ]
  --   , [ createEnemy (EString "cc") Upper | _ <- [1..4] ]
  --   , [ createEnemy (EChar 'E') Lower | _ <- [1..3] ]
  --   , [ createEnemy (EString "dz") Upper | _ <- [1..4] ]
  --   , [ createEnemy (EList [EInt 1, EInt 2, EInt 3]) Upper ]
  --   , [ createEnemy (EMap [("key1", EInt 42), ("key2", EChar 'x')]) Lower ]
  --   ]
  -- , -- Wave 2
  --   [ [ createEnemy (EChar 'B') Lower | _ <- [1..4] ]
  --   , [ createEnemy (EInt 2) Upper | _ <- [1..3] ]
  --   , [ createEnemy (EString "bb") Lower | _ <- [1..4] ]
  --   , [ createEnemy (EChar 'C') Upper | _ <- [1..3] ]
  --   ]
  -- , -- Wave 3
  --   [ [ createEnemy (EChar 'A') Upper | _ <- [1..3] ]
  --   , [ createEnemy (EInt 1) Lower | _ <- [1..5] ]
  --   , [ createEnemy (EString "hello") Upper | _ <- [1..3] ]
  --   ]
  -- , -- Boss Wave (Wave 4)
    [ [ createBoss Upper ] ]
  ]