module Game.Config where

-- Logical map dimensions
mapWidth, mapHeight :: Float
mapWidth = 600
mapHeight = 400

-- Window dimensions (slightly larger)
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- Offsets
mapOffsetY :: Float
mapOffsetY = -20  -- so that text is ~5 px above

textAboveMapY :: Float
textAboveMapY = mapHeight / 2 + 5
