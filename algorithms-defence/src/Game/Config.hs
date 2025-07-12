module Game.Config where
import Game.Types
import Graphics.Gloss

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

 -- Tower placement spots: Xs near the path
towerSpots :: Picture
towerSpots = color black $ pictures $ map drawX positions
  where
    drawX (x, y) = translate x y $ scale 0.15 0.15 $ text "x"
    positions =
      [ (-100, 20), (-100, -30)
      , (-30, 30),  (-30, -50)
      , (40, 60),   (40, -80)
      , (100, 75),  (100, -90)
      , (150, 50),  (150, -60),
        (-10, -5)
      ]
