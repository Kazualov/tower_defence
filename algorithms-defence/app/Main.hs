module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game.Types       -- contains GameState, Enemy, EnemyType definitions
import Game.Render      -- contains drawScene :: GameState -> Picture
import Game.Enemies     
import Game.Logic       -- contains updateEnemies :: Float -> [Enemy] -> [Enemy]


-- Main entry point
main :: IO ()
main = play
  (InWindow "Tower Defense" (800, 600) (100, 100))  -- window setup
  white                                             -- background color
  60                                                -- frames per second
  initialState                                      -- initial game state
  drawScene                                         -- render function
  handleInput                                       -- input handler
  updateGame                                        -- update function

-- Initial game state
initialState :: GameState
initialState = GameState
  { enemies =
      [ Enemy (EChar 'x') (-50, 250)
      , Enemy (EInt 42) (0, 250)
      , Enemy (EString "List") (50, 250)
      ]
  }

-- No input handling for now
handleInput :: Event -> GameState -> GameState
handleInput _ gs = gs

-- Game update logic
updateGame :: Float -> GameState -> GameState
updateGame dt gs = gs { enemies = updateEnemies dt (enemies gs) }
