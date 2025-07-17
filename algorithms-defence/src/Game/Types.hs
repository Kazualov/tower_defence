module Game.Types where

import System.Random (StdGen)
import Graphics.Gloss (Picture)


type Position = (Float, Float)

type TowerSpot = (Float, Float)

data Tower = Tower
  { towerType     :: TowerType
  , towerPos      :: TowerSpot
  , towerCooldown :: Float  -- seconds remaining until next shot
  , towerTarget   :: Maybe Enemy
  } deriving (Show, Eq)


data TowerType = Archer | Cannon | Sniper
  deriving (Show, Eq)

-- Intro Float - time from the start of the intro
data GameStatus = Intro Int Float  -- Now stores current image index and time
                | Playing 
                | Victory 
                | Defeat
  deriving (Show, Eq)


data GameState = GameState
  { towerHP     :: Int
  , doodleText  :: String
  , enemies     :: [Enemy]
  , introImages :: [Picture]
  , currentWave :: Int
  , waveQueue   :: [[[Enemy]]]
  , currentGroup :: Maybe ([Enemy], Int)
  , enemySpawnTimer :: Float
  , groupSpawnTimer :: Float
  , wavePauseTimer  :: Float
  , randomGen   :: StdGen
  , towers      :: [Tower]
  , towerSpots  :: [TowerSpot]
  , selectedTower :: TowerType
  , coins       :: Int
  , gameStatus  :: GameStatus
  , isPaused    :: Bool
  , showPauseMenu :: Bool
  } deriving (Show)

data EnemyType
  = EChar Char
  | EInt Int
  | EString String
  | EList [EnemyType]     -- Linked List enemy: represents nested elements
  | EMap [(String, EnemyType)]  -- Map enemy: represents key-value structure
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


data WaveParams = WaveParams
  { waveSize :: Int          -- Number of enemies in wave
  , groupCount :: Int        -- Number of groups
  , intensity :: Float       -- 0-1 scale of difficulty
  } deriving (Show)