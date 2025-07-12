-- Updated Game.Types
module Game.Types where

type Position = (Float, Float)

data GameState = GameState
  { towerHP     :: Int
  , doodleText  :: String
  , enemies :: [Enemy]
  }

data EnemyType = EChar Char | EInt Int | EString String
  deriving (Show, Eq)

data Path = Upper | Lower
  deriving (Show, Eq)  

data Enemy = Enemy
  { enemyType       :: EnemyType
  , enemyPosition   :: Position
  , enemyWaypointIx :: Int
  , enemyPath       :: Path
  }

