data EnemyType = EChar Char | EInt Int | EString String
  deriving (Show, Eq)

data Enemy = Enemy
  { enemyType :: EnemyType
  , position  :: (Float, Float)  -- x, y coordinates
  }
