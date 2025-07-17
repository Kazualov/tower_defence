module Game.Config where

import Game.Types

coinRewardPerKill :: Int
coinRewardPerKill = 10

cellSize :: Float
cellSize = 20

colToX :: Int -> Float
colToX col = -fromIntegral windowWidth / 2 + cellSize / 2 + fromIntegral col * cellSize

rowToY :: Int -> Float
rowToY row = -fromIntegral windowHeight / 2 + cellSize / 2 + fromIntegral row * cellSize

-- Logical map dimensions
mapWidth, mapHeight :: Float
mapWidth = 800
mapHeight = 400

-- Radius of area attacking by cannon
blastRadius :: Float
blastRadius = 30

-- Window dimensions
windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 800

-- Offsets
mapOffsetY :: Float
mapOffsetY = -20

textAboveMapY :: Float
textAboveMapY = mapHeight / 2 + 50

-- Position of main tower
mainTowerPos :: (Float, Float)
mainTowerPos = (-mapWidth / 2 + 40, -45)

-- The start of the top branch
topPathStart :: Position
topPathStart = (mapWidth / 2 - 30, 45)

-- The start of the botton branch
bottomPathStart :: Position
bottomPathStart = (mapWidth / 2 - 30, -90)

-- The start of the tunnel
tunnelStart :: (Float, Float)
tunnelStart = (-mapWidth/2 + 350, -45)

upperPathWaypoints :: [Position]
upperPathWaypoints =
  [ 
  topPathStart
  , (mapWidth/2 - 30, 60), (120, 25), (80, 10), (0, -10), (-10, -20)
  , tunnelStart
  , mainTowerPos 
  ]

lowerPathWaypoints :: [Position]
lowerPathWaypoints =
  [ 
  bottomPathStart
  , (mapWidth/2 - 30, -90), (80, -60), (0, -45), (-50, -40)
  , tunnelStart
  , mainTowerPos 
  ]

enemyDelay :: Float
enemyDelay = 0.5

groupDelay :: Float
groupDelay = 2.0

waveDelay :: Float
waveDelay = 5.0

enemySpeed :: Float
enemySpeed = 30

towerCost :: TowerType -> Int
towerCost Archer = 30
towerCost Cannon = 50
towerCost Sniper = 80

-- NEW: Modificator costs
modificatorCost :: Modificator -> Int
modificatorCost Map = 100
modificatorCost Filter = 500
modificatorCost GarbageCollector = 80

cooldownFor :: TowerType -> Float
cooldownFor Archer = 0.5
cooldownFor Cannon = 1.0
cooldownFor Sniper = 2.0

towerDamageFor :: TowerType -> Int
towerDamageFor Archer = 20
towerDamageFor Cannon = 10
towerDamageFor Sniper = 80

towerDamageForMap :: TowerType -> Int
towerDamageForMap Archer = 10  -- reduced damage
towerDamageForMap Cannon = 20
towerDamageForMap Sniper = 40

mapModRadius :: Float
mapModRadius = 60

towerRangeFor :: TowerType -> Float
towerRangeFor Archer = 150  -- radius in pixels
towerRangeFor Cannon = 100
towerRangeFor Sniper = 250

hpOf :: EnemyType -> Int
hpOf (EChar _)   = 100
hpOf (EInt _)    = 200
hpOf (EString _) = 300
hpOf (EList xs)   = 100 + sum (map hpOf xs)  -- Optional: make it recursive
hpOf (EMap kvs) = 50 + sum (map (hpOf . snd) kvs)

shopButtonMinY, shopButtonMaxY :: Float
shopButtonMinY = -fromIntegral windowHeight / 2 + 40 - 50  -- shop y - half height
shopButtonMaxY = -fromIntegral windowHeight / 2 + 40 + 50  -- shop y + half height

shopButtonXOffsets :: [Float]
shopButtonXOffsets = [startX + fromIntegral i * buttonSpacing | i <- [0..2]]
  where
    startX = -250
    buttonSpacing = 100

shopButtonWidth :: Float
shopButtonWidth = 180

towerAtClick :: Float -> Maybe TowerType
towerAtClick mx =
  case filter (\(i, x) -> abs (mx - x) <= shopButtonWidth / 2)
              (zip [0..] shopButtonXOffsets) of
    ((0, _):_) -> Just Archer
    ((1, _):_) -> Just Cannon
    ((2, _):_) -> Just Sniper
    _          -> Nothing