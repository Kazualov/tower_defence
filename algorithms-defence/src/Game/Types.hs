-- Updated Game.Types
module Game.Types where

type Position = (Float, Float)

type TowerSpot = (Float, Float)

type Tower = (TowerType, TowerSpot)

data TowerType = Archer | Cannon | Sniper
  deriving (Show, Eq)


data GameState = GameState
  { towerHP     :: Int
  , doodleText  :: String
  , enemies :: [Enemy]
  , towers      :: [Tower]
  , towerSpots  :: [TowerSpot]
  , selectedTower :: TowerType
  } deriving (Show)

data EnemyType = EChar Char | EInt Int | EString String
  deriving (Show, Eq)

data Enemy = Enemy
  { enemyType :: EnemyType
  , position  :: (Float, Float)
  , currentWaypoint :: Int  -- Index of the current target waypoint
  , health :: Int
  } deriving (Show, Eq)

-- Define the path waypoints (adjust coordinates based on your actual layout)
pathWaypoints :: [Position]
pathWaypoints = 
  [ (-580, 0)    -- Start (spawn point)
  , (-400, 0)    -- First turn
  , (-400, 200)  -- Going up
  , (-100, 200)  -- Going right
  , (-100, -200) -- Going down
  , (200, -200)  -- Going right
  , (200, 100)   -- Going up
  , (500, 100)   -- Going right
  , (500, -100)  -- Going down
  , (580, -100)  -- End (base)
  ]