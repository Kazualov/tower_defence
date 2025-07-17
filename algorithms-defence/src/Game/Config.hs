module Game.Config where

import Game.Types

-- Duration of a single intro image (in seconds)
imageDuration :: Float
imageDuration = 3.0

-- Total time the intro should play (in seconds)
totalIntroTime :: Float
totalIntroTime = 5.0

-- Difficulty scaling factor for waves
difficultyMultiplier :: Float
difficultyMultiplier = 0.2

-- Coins earned per enemy kill
coinRewardPerKill :: Int
coinRewardPerKill = 10

-- Size of a single cell in the grid (used for positioning)
cellSize :: Float
cellSize = 20

-- Convert column index to x-position on screen
colToX :: Int -> Float
colToX col = -fromIntegral windowWidth / 2 + cellSize / 2 + fromIntegral col * cellSize

-- Convert row index to y-position on screen
rowToY :: Int -> Float
rowToY row = -fromIntegral windowHeight / 2 + cellSize / 2 + fromIntegral row * cellSize

-- Logical size of the game map
mapWidth, mapHeight :: Float
mapWidth = 800
mapHeight = 400

-- Radius of cannon's area-of-effect damage
blastRadius :: Float
blastRadius = 30

-- Screen dimensions
windowWidth, windowHeight :: Int
windowWidth = 1300
windowHeight = 750

-- Vertical offset of the map on the screen
mapOffsetY :: Float
mapOffsetY = -20

-- Y-position for text displayed above the map
textAboveMapY :: Float
textAboveMapY = mapHeight / 2 + 50

-- Position of the main tower (final target)
mainTowerPos :: (Float, Float)
mainTowerPos = (-mapWidth / 2 + 40, -45)

-- Starting position of the upper enemy path
topPathStart :: Position
topPathStart = (mapWidth / 2 - 30, 45)

-- Starting position of the lower enemy path
bottomPathStart :: Position
bottomPathStart = (mapWidth / 2 - 30, -90)

-- Starting position of the tunnel section in both paths
tunnelStart :: (Float, Float)
tunnelStart = (-mapWidth / 2 + 350, -45)

-- Waypoints for the upper path
upperPathWaypoints :: [Position]
upperPathWaypoints =
  [ topPathStart
  , (mapWidth / 2 - 30, 60), (120, 25), (80, 10), (0, -10), (-10, -20)
  , tunnelStart
  , mainTowerPos
  ]

-- Waypoints for the lower path
lowerPathWaypoints :: [Position]
lowerPathWaypoints =
  [ bottomPathStart
  , (mapWidth / 2 - 30, -90), (80, -60), (0, -45), (-50, -40)
  , tunnelStart
  , mainTowerPos
  ]

-- Delay between enemy spawns within a group (seconds)
enemyDelay :: Float
enemyDelay = 1.5

-- Delay between groups in a wave (seconds)
groupDelay :: Float
groupDelay = 2

-- Delay between waves (seconds)
waveDelay :: Float
waveDelay = 5.0

-- Base speed of regular enemies (pixels/second)
enemySpeed :: Float
enemySpeed = 30

-- Cost of building each type of tower
towerCost :: TowerType -> Int
towerCost Archer = 30
towerCost Cannon = 50
towerCost Sniper = 80

-- Cost of each modificator
modificatorCost :: Modificator -> Int
modificatorCost Map = 100
modificatorCost Pop = 500
modificatorCost GarbageCollector = 80

-- Cooldown (delay between attacks) for each tower type
cooldownFor :: TowerType -> Float
cooldownFor Archer = 0.5
cooldownFor Cannon = 1.0
cooldownFor Sniper = 2.0

-- Damage dealt by each tower type
towerDamageFor :: TowerType -> Int
towerDamageFor Archer = 20
towerDamageFor Cannon = 10
towerDamageFor Sniper = 80

-- Damage dealt by each tower type when affected by the Map modificator
towerDamageForMap :: TowerType -> Int
towerDamageForMap Archer = 10  -- reduced damage
towerDamageForMap Cannon = 20
towerDamageForMap Sniper = 40

-- Radius of effect for Map modificator
mapModRadius :: Float
mapModRadius = 60

-- Attack range (in pixels) for each tower type
towerRangeFor :: TowerType -> Float
towerRangeFor Archer = 150
towerRangeFor Cannon = 100
towerRangeFor Sniper = 250

-- Boss parameters
bossHealth :: Int
bossHealth = 1000

bossSpeed :: Float
bossSpeed = 20  -- Slower than normal enemies

bossChildHealth :: Int
bossChildHealth = 700

bossChildOffset :: Float
bossChildOffset = 40  -- Distance between children to avoid overlap

bossDamage :: Int
bossDamage = 100  -- Damage boss deals to the main tower

bossChildDelay :: Float
bossChildDelay = 5.0  -- Time between spawning boss children

-- Compute health points for an enemy based on its type
hpOf :: EnemyType -> Int
hpOf (EChar _)    = 100
hpOf (EInt _)     = 200
hpOf (EString _)  = 300
hpOf (EList xs)   = 100 + sum (map hpOf xs)
hpOf (EMap kvs)   = 50 + sum (map (hpOf . snd) kvs)
hpOf Boss         = bossHealth

-- Vertical bounds for tower selection buttons
shopButtonMinY, shopButtonMaxY :: Float
shopButtonMinY = -fromIntegral windowHeight / 2 + 40 - 50
shopButtonMaxY = -fromIntegral windowHeight / 2 + 40 + 50

-- Horizontal positions of tower selection buttons
shopButtonXOffsets :: [Float]
shopButtonXOffsets = [startX + fromIntegral i * buttonSpacing | i <- [0..2]]
  where
    startX = -250
    buttonSpacing = 100

-- Width of each shop button (in pixels)
shopButtonWidth :: Float
shopButtonWidth = 180

-- Determine which tower was clicked based on x-position
towerAtClick :: Float -> Maybe TowerType
towerAtClick mx =
  case filter (\(i, x) -> abs (mx - x) <= shopButtonWidth / 2)
              (zip [0..] shopButtonXOffsets) of
    ((0, _):_) -> Just Archer
    ((1, _):_) -> Just Cannon
    ((2, _):_) -> Just Sniper
    _          -> Nothing
