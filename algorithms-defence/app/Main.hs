module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game.Types -- contains GameState, Enemy, EnemyType definitions
import Game.Render
import Game.Logic -- contains updateEnemies :: Float -> [Enemy] -> [Enemy]
import Game.Config
import Game.Enemies -- Import the helper function
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
  , enemies = []
  , towers = []  -- no towers at start
  , towerSpots =  -- same positions used by drawXs in Game.Shapes
      [ (-275, 10), (-275, -70)
      , (-175, 10),  (-175, -70)
      , (-75, 10),   (-75, -70)
      , (25, 40),  (25, -105)
      , (125, 70),  (125, -120)
      , (50, -20)
      ]
  , selectedTower = Archer
  , currentWave = 0
  , waveQueue = generateWaves !! 0
  , currentGroup = Nothing
  , enemySpawnTimer = 0
  , groupSpawnTimer = 0
  , wavePauseTimer = 0
  }

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char '1') Down _ _) gs = gs { selectedTower = Archer }
handleInput (EventKey (Char '2') Down _ _) gs = gs { selectedTower = Cannon }
handleInput (EventKey (Char '3') Down _ _) gs = gs { selectedTower = Sniper }
handleInput (EventKey (MouseButton LeftButton) Up _ clickPos) gs =
  tryPlaceTower clickPos gs
handleInput _ gs = gs

-- updateGame :: Float -> GameState -> GameState
-- updateGame dt gs = gs { enemies = updateEnemies dt (enemies gs) }

tryPlaceTower :: (Float, Float) -> GameState -> GameState
tryPlaceTower click gs =
  case find (isClose click) (towerSpots gs) of
    Just spot ->
      let newTower = Tower (selectedTower gs) spot 0.0
          remainingSpots = filter (/= spot) (towerSpots gs)
      in gs { towers = newTower : towers gs, towerSpots = remainingSpots }
    Nothing -> gs
  where
    isClose (x1, y1) (x2, y2) = abs (x1 - x2) < 20 && abs (y1 - y2) < 20



updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let gs1 = updateWaveSystem dt gs
      movedEnemies = updateEnemies dt (enemies gs1)
      (updatedTowers, damagedEnemies) = applyTowerDamage (towers gs1) movedEnemies
      aliveEnemies = filter (\e -> health e > 0) damagedEnemies
      cooledTowers = updateTowersCooldown dt updatedTowers
  in gs1 { enemies = aliveEnemies, towers = cooledTowers }


