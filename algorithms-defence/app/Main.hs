module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game.Types -- contains GameState, Enemy, EnemyType definitions
import Game.Render
import Game.Logic -- contains updateEnemies :: Float -> [Enemy] -> [Enemy]
import Game.Config
import Game.Enemies (createEnemy) -- Import the helper function
import Data.List (find)


-- Main entry point
main :: IO ()
main = play
  (InWindow "Haskell Tower Defense" (windowWidth, windowHeight) (100, 100))
  white
  60
  initialState
  render
  handleInput
  updateGame

-- Initial game state
initialState :: GameState
initialState = GameState
  { towerHP = 100
  , doodleText = "Hello"
  , enemies =
    [ createEnemy (EChar 'x')      -- Creates enemy at spawn with waypoint 0
    , createEnemy (EInt 42)        -- Creates enemy at spawn with waypoint 0
    , createEnemy (EString "List") -- Creates enemy at spawn with waypoint 0
    ]
  , towers = []  -- no towers at start
  , towerSpots =  -- same positions used by drawXs in Game.Shapes
      [ (-100, 20), (-100, -30)
      , (-30, 30),  (-30, -50)
      , (40, 60),   (40, -80)
      , (100, 75),  (100, -90)
      , (150, 50),  (150, -60)
      , (-10, -5)
      ]
  , selectedTower = Archer
  }

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char '1') Down _ _) gs = gs { selectedTower = Archer }
handleInput (EventKey (Char '2') Down _ _) gs = gs { selectedTower = Cannon }
handleInput (EventKey (Char '3') Down _ _) gs = gs { selectedTower = Sniper }
handleInput (EventKey (MouseButton LeftButton) Up _ clickPos) gs =
  tryPlaceTower clickPos gs
handleInput _ gs = gs


tryPlaceTower :: (Float, Float) -> GameState -> GameState
tryPlaceTower click gs =
  case find (isClose click) (towerSpots gs) of
    Just spot ->
      let newTower = (selectedTower gs, spot)
          remainingSpots = filter (/= spot) (towerSpots gs)
      in gs { towers = newTower : towers gs, towerSpots = remainingSpots }
    Nothing -> gs
  where
    isClose (x1, y1) (x2, y2) = abs (x1 - x2) < 20 && abs (y1 - y2) < 20



-- Game update logic
updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let movedEnemies = updateEnemies dt (enemies gs)
      damagedEnemies = applyTowerDamage (towers gs) movedEnemies
      aliveEnemies = filter (\e -> health e > 0) damagedEnemies
  in gs { enemies = aliveEnemies }
