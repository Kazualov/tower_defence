module Game.Config where


import Game.Types
import Graphics.Gloss


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

-- Window dimensions (slightly larger)
windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 600

-- Offsets
mapOffsetY :: Float
mapOffsetY = -20 -- so that text is ~5 px above

textAboveMapY :: Float
textAboveMapY = mapHeight / 2 + 50

-- Position of main tower
mainTowerPos :: (Float, Float)
mainTowerPos = (-mapWidth / 2 + 40, -45)

-- Стартовая точка верхней ветки
topPathStart :: Position
topPathStart = (mapWidth / 2 - 30, 45)

-- Стартовая точка нижней ветки -- todo
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
