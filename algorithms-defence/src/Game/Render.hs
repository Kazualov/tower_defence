module Game.Render where

import Graphics.Gloss

import Game.Config
import Game.Shapes hiding (towerSpots)
import Game.Enemies
import Game.Logic ()
import Game.Types

render :: GameState -> Picture
render gs = Pictures
  [ -- Centered map group
    translate 0 mapOffsetY $ pictures
      [ mapBorder
      , lambdaPath
      , drawTowerSpots (towerSpots gs)  -- updated: draws only unoccupied Xs
      , mainTower
      , translate (-(mapWidth/2) + 0) (textAboveMapY + mapOffsetY) $
      scale 0.2 0.2 $ color black $ text "You may draw your doodles here:"
      ]
      , drawEnemies (enemies gs)   -- enemies come from GameState now
      , drawPlacedTowers (towers gs)
      , translate (-mapWidth/2) (-mapHeight/2 - 50) $
        scale 0.15 0.15 $
        color black $
        text ("Selected: " ++ show (selectedTower gs))
      , renderWaveIndicator gs
  ]

drawEnemies :: [Enemy] -> Picture 
drawEnemies = Pictures . map drawEnemy

drawPlacedTowers :: [Tower] -> Picture
drawPlacedTowers = pictures . map drawTowerAtScreen

drawTowerAtScreen :: Tower -> Picture
drawTowerAtScreen (Tower ttype pos _) =
  translate x y $
    case ttype of
      Archer -> drawArcherTowerRaw
      Cannon -> drawCannonTowerRaw
      Sniper -> drawSniperTowerRaw
  where (x, y) = pos


-- Convert screen coords to grid cells
xToCol :: Float -> Int
xToCol x = round (x / cellSize)

yToRow :: Float -> Int
yToRow y = round (y / cellSize)

drawTowerSpots :: [TowerSpot] -> Picture
drawTowerSpots = color black . pictures . map drawX
  where
    drawX (x, y) = translate x y $ scale 0.15 0.15 $ text "x"

renderWaveIndicator :: GameState -> Picture
renderWaveIndicator gs =
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 30)
    $ scale 0.15 0.15
    $ color black
    $ text ("Wave: " ++ show (currentWave gs + 1))

