module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game.Types -- contains GameState, Enemy, EnemyType definitions
import Game.Render
import Game.Logic -- contains updateEnemies :: Float -> [Enemy] -> [Enemy]
import Game.Config
import Game.Enemies (createEnemy, moveEnemy) -- Import the helper function

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
    [ createEnemy (EChar 'x') Upper
    , createEnemy (EInt 4) Lower
    ]
  }

-- No input handling for now
handleInput :: Event -> GameState -> GameState
handleInput _ gs = gs

updateGame :: Float -> GameState -> GameState
updateGame dt gs = gs { enemies = updateEnemies dt (enemies gs) }

