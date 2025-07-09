module Game.Types where

type Position = (Float, Float)

data GameState = GameState
  { towerHP     :: Int
  , doodleText  :: String
  }
