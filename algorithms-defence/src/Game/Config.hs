module Game.Config where
import Game.Types

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
mainTowerPos = (-mapWidth / 2 + 40, -20)

topPathStart :: (Float, Float)
topPathStart = (mapWidth/2 - 30, 90 / 2)

-- Define the path waypoints (adjust coordinates based on your actual layout)
topPathWaypoints :: [Position]
topPathWaypoints =
 [ (-580, 0) -- Start (spawn point)
 , (-400, 0) -- First turn
 , (-400, 200) -- Going up
 , (-100, 200) -- Going right
 , (-100, -200) -- Going down
 , (200, -200) -- Going right
 , (200, 100) -- Going up
 , (500, 100) -- Going right
 , (500, -100) -- Going down
 , (580, -100) -- End (base)
 ]