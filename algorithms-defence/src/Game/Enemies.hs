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

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy etype (x, y) _ _ hp) =
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
enemyLabel (EList xs)     = "[" ++ joinWith ", " (map enemyLabel xs) ++ "]"
enemyLabel (EMap kvs)     = "{" ++ joinWith ", " (map showKV kvs) ++ "}"
  where
    showKV (k, v) = k ++ ": " ++ enemyLabel v

-- Function for building lists and maps representation in the game
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
moveEnemy speed enemy@(Enemy etype (x, y) waypointIndex path hp)
  | waypointIndex >= length waypoints = enemy
  | otherwise =
      let (tx, ty) = waypoints !! waypointIndex
          dx = tx - x
          dy = ty - y
          dist = sqrt (dx * dx + dy * dy)
          waypointReached = dist < 5.0
          newWaypointIndex = if waypointReached then waypointIndex + 1 else waypointIndex

          (nx, ny)
            | waypointReached
            , newWaypointIndex < length waypoints =
                let (nextTx, nextTy) = waypoints !! newWaypointIndex
                    nextDx = nextTx - x
                    nextDy = nextTy - y
                    nextDist = sqrt (nextDx * nextDx + nextDy * nextDy)
                in if nextDist < 1
                   then (x, y)
                   else (x + speed * nextDx / nextDist, y + speed * nextDy / nextDist)

            | dist < 1 = (x, y)

            | otherwise = (x + speed * dx / dist, y + speed * dy / dist)

      in Enemy etype (nx, ny) newWaypointIndex path hp
  where
    waypoints = pathWaypoints path


-- Wave definitions using list comprehensions:
generateWaves :: [[ [Enemy] ]]
generateWaves =
  [ [ [ createEnemy (EChar 'D') Upper | _ <- [1..4] ]
    , [ createEnemy (EInt 3) Lower | _ <- [1..4] ]
    , [ createEnemy (EString "c") Upper | _ <- [1..4] ]
    , [ createEnemy (EChar 'E') Lower | _ <- [1..3] ]
    , [ createEnemy (EString "d") Upper | _ <- [1..4] ]
    , [ createEnemy (EList [EInt '1', EInt '2', EInt '3']) Upper ]
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
  ]
