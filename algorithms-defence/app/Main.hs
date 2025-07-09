module Main where

import Graphics.Gloss.Interface.Pure.Game (Event)
import Graphics.Gloss
import Game.Types
import Game.Config
import Game.Render

main :: IO ()
main = play
  (InWindow "Haskell Tower Defense" (windowWidth, windowHeight) (100, 100))
  white
  60
  initialState
  render
  handleInput
  update

initialState :: GameState
initialState = GameState { towerHP = 100, doodleText = "" }

handleInput :: Event -> GameState -> GameState
handleInput _ state = state  -- No interaction yet

update :: Float -> GameState -> GameState
update _ state = state       -- No game logic yet
