module Game.Types where

type Position = (Float, Float)

type TowerSpot = (Float, Float)

-- Modificator types
data Modificator = Map | Filter | GarbageCollector
  deriving (Show, Eq)

data Tower = Tower
  { towerType     :: TowerType
  , towerPos      :: TowerSpot
  , towerCooldown :: Float  -- seconds remaining until next shot
  , towerTarget   :: Maybe Enemy
  , towerMod      :: Maybe Modificator  -- NEW: optional modificator
  } deriving (Show, Eq)

data TowerType = Archer | Cannon | Sniper
  deriving (Show, Eq)

-- Intro Float - time from the start of the intro
data GameStatus = Intro Float | Playing | Victory | Defeat
  deriving (Show, Eq)

data GameState = GameState
  { towerHP     :: Int
  , doodleText  :: String
  , enemies :: [Enemy]
  -- Waves
  , currentWave :: Int
  , waveQueue :: [[Enemy]]       -- groups left to spawn in current wave
  , currentGroup :: Maybe ([Enemy], Int) -- (enemies in group, enemies spawned count)
  , enemySpawnTimer :: Float
  , groupSpawnTimer :: Float
  , wavePauseTimer :: Float
  -- Waves
  , towers      :: [Tower]
  , towerSpots  :: [TowerSpot]
  , selectedTower :: TowerType
  , coins       :: Int
  , gameStatus      :: GameStatus
  , isPaused    :: Bool
  , showPauseMenu :: Bool
  , selectedModificator :: Maybe Modificator
  , selectedTowerForMod :: Maybe Tower  -- ← Restore this
  } deriving (Show)

data EnemyType = EChar Char | EInt Int | EString String
  deriving (Show, Eq)

data Path = Upper | Lower
  deriving (Show, Eq)  

data Enemy = Enemy
  { enemyType       :: EnemyType
  , enemyPosition   :: Position
  , enemyWaypointIx :: Int
  , enemyPath       :: Path
  , health          :: Int
  } deriving (Show, Eq)