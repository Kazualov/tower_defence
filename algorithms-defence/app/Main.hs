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
      let newTower = Tower (selectedTower gs) spot 0.0
          remainingSpots = filter (/= spot) (towerSpots gs)
      in gs { towers = newTower : towers gs, towerSpots = remainingSpots }
    Nothing -> gs
  where
    isClose (x1, y1) (x2, y2) = abs (x1 - x2) < 20 && abs (y1 - y2) < 20



-- Game update logic
updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let movedEnemies = updateEnemies dt (enemies gs)
      (updatedTowers, damagedEnemies) = applyTowerDamage (towers gs) movedEnemies
      aliveEnemies = filter (\e -> health e > 0) damagedEnemies
      cooledTowers = updateTowersCooldown dt updatedTowers
  in gs { enemies = aliveEnemies, towers = cooledTowers }

