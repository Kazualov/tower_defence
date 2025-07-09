module Game.Types where

data EnemyType = EChar Char | EInt Int | EString String
  deriving (Show, Eq)

data Enemy = Enemy
  { enemyType :: EnemyType
  , position  :: (Float, Float)
  } deriving (Show, Eq)

data GameState = GameState
  { enemies :: [Enemy]
  -- you can add more fields like towers, score, etc.
  } deriving (Show)