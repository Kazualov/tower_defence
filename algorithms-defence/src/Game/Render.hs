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
      , renderCoins gs
      , renderTowerPrices
  ]

drawEnemies :: [Enemy] -> Picture 
drawEnemies = Pictures . map drawEnemy

drawPlacedTowers :: [Tower] -> Picture
drawPlacedTowers = pictures . map drawTowerAtScreen

drawTowerAtScreen :: Tower -> Picture
drawTowerAtScreen (Tower ttype pos _ _) =
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

renderCoins :: GameState -> Picture
renderCoins gs =
  translate (fromIntegral windowWidth / 2 - 150)
            (fromIntegral windowHeight / 2 - 30) $
    Pictures
      [ -- Draw the coin icon (yellow circle)
        translate 113 8 $
          color (makeColorI 255 215 0 255) $  -- gold/yellow
            thickCircle 7 5

      , -- Draw the coin count text to the right of the icon
        translate 0 0 $  -- shift text slightly right and down
          scale 0.15 0.15 $
            color black $
              text ("Coins: " ++ show (coins gs))
      ]



renderTowerPrices :: Picture
renderTowerPrices =
  translate (fromIntegral windowWidth / 2 - 150)
            (fromIntegral windowHeight / 2 - 55) $
    Pictures $
      zipWith drawLine [0..] linesOfText
  where
    -- Each tuple: (line text, coin offset for the icon)
    linesOfText =
      [ ("Tower Prices:", Nothing)  -- No icon for the title
      , ("Archer: " ++ show (towerCost Archer), Just 80)
      , ("Cannon: " ++ show (towerCost Cannon), Just 80)
      , ("Sniper: " ++ show (towerCost Sniper), Just 80)
      ]

    drawLine :: Int -> (String, Maybe Float) -> Picture
    drawLine i (lineText, mIconOffset) =
      let yOffset = -20 * fromIntegral i
          textPic = translate 0 yOffset $
                    scale 0.12 0.12 $
                      color black $
                        text lineText
          coinPic = case mIconOffset of
            Just x -> translate 95 (yOffset + 8) $
                        color (makeColorI 255 215 0 255) $
                          thickCircle 5 3
            Nothing -> Blank
      in Pictures [textPic, coinPic]

