module Game.Types where

import System.Random (StdGen)
import Graphics.Gloss (Picture)

-- | 2D coordinate used for positions
type Position = (Float, Float)

-- | Possible tower placement coordinates on the map
type TowerSpot = (Float, Float)

-- | Power-ups (modificators) that enhance tower capabilities
data Modificator
  = Map                -- Affects area/detection
  | Pop                -- Instantly pops an enemy
  | GarbageCollector   -- Clears multiple weak enemies
  deriving (Show, Eq)

-- | A single tower on the field
data Tower = Tower
  { towerType     :: TowerType           -- Type of the tower (e.g., Archer)
  , towerPos      :: TowerSpot           -- Location of the tower
  , towerCooldown :: Float               -- Time left before it can shoot again
  , towerTarget   :: Maybe Enemy         -- Current target (if any)
  , towerMod      :: Maybe Modificator   -- Optional applied modificator
  } deriving (Show, Eq)

-- | The three tower types available in the game
data TowerType = Archer | Cannon | Sniper
  deriving (Show, Eq)

-- | Current state of the game (used for control flow)
data GameStatus
  = Intro Int Float  -- Showing intro image with index and elapsed time
  | Playing          -- Active gameplay
  | Victory          -- Player won
  | Defeat           -- Player lost
  deriving (Show, Eq)

-- | Complete game state stored and updated each frame
data GameState = GameState
  { towerHP               :: Int                     -- Player's base health
  , doodleText            :: String                  -- Debug/test text (can be used for UI)
  , enemies               :: [Enemy]                 -- All current enemies on screen
  , introImages           :: [Picture]               -- Slideshow images for intro
  , currentWave           :: Int                     -- Index of the current wave
  , waveQueue             :: [[[Enemy]]]             -- All future waves (nested groups)
  , currentGroup          :: Maybe ([Enemy], Int)    -- Current enemy group + delay
  , enemySpawnTimer       :: Float                   -- Timer until next enemy spawns
  , groupSpawnTimer       :: Float                   -- Timer until next group spawns
  , wavePauseTimer        :: Float                   -- Pause between waves
  , randomGen             :: StdGen                  -- RNG state for spawning randomness
  , towers                :: [Tower]                 -- All player-built towers
  , towerSpots            :: [TowerSpot]             -- Remaining open spots for towers
  , selectedTower         :: TowerType               -- Currently selected tower for placement
  , coins                 :: Int                     -- Player's in-game currency
  , gameStatus            :: GameStatus              -- Current game flow status
  , isPaused              :: Bool                    -- Whether the game is paused
  , showPauseMenu         :: Bool                    -- Whether pause menu UI is visible
  , selectedModificator   :: Maybe Modificator       -- Modificator picked for use
  , selectedTowerForMod   :: Maybe Tower             -- Tower being targeted for a mod
  } deriving (Show)

-- | Types of enemies in the game
data EnemyType
  = EChar Char                    -- Enemy represented by a single character
  | EInt Int                      -- Enemy represented by an integer
  | EString String                -- Enemy with string label
  | EList [EnemyType]            -- Nested enemy types
  | EMap [(String, EnemyType)]   -- Complex structured enemy (like a record/map)
  | Boss                         -- Special strong enemy at the end
  deriving (Show, Eq)

-- | Path determines which route the enemy takes (upper or lower side)
data Path = Upper | Lower
  deriving (Show, Eq)

-- | Individual enemy instance on the map
data Enemy = Enemy
  { enemyType       :: EnemyType    -- Logical type of the enemy
  , enemyPosition   :: Position     -- Current position on the screen
  , enemyWaypointIx :: Int          -- Index of the next waypoint in the path
  , enemyPath       :: Path         -- Which path the enemy follows
  , health          :: Int          -- Current health of the enemy
  } deriving (Show, Eq)

-- | Settings used to generate a wave of enemies
data WaveParams = WaveParams
  { waveSize   :: Int     -- Total number of enemies in the wave
  , groupCount :: Int     -- Number of groups (sub-waves)
  , intensity  :: Float   -- Difficulty level (0.0 to 1.0)
  } deriving (Show)
