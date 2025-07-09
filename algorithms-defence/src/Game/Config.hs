module Game.Config where

cellSize :: Float
cellSize = 20

gridWidth, gridHeight :: Int
gridWidth = 35
gridHeight = 25

windowWidth, windowHeight :: Int
windowWidth  = round (cellSize * fromIntegral gridWidth)
windowHeight = round (cellSize * fromIntegral gridHeight)

colToX :: Int -> Float
colToX col = -fromIntegral windowWidth / 2 + cellSize / 2 + fromIntegral col * cellSize

rowToY :: Int -> Float
rowToY row = -fromIntegral windowHeight / 2 + cellSize / 2 + fromIntegral row * cellSize
