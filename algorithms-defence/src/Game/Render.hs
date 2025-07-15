module Game.Render where

import Graphics.Gloss

import Game.Shop (renderShopMenu)
import Game.Config
import Game.Shapes hiding (towerSpots)
import Game.Enemies
import Game.Logic ()
import Game.Types

render :: GameState -> Picture
render gs = case gameStatus gs of
  Intro t -> renderIntro t
  _       -> renderGame gs

renderGame :: GameState -> Picture
renderGame gs = Pictures
  [ -- (unchanged: full game scene below)
    translate 0 mapOffsetY $ pictures
      [ mapBorder
      , lambdaPath
      , drawTowerSpots (towerSpots gs)
      , mainTower
      , translate (-(mapWidth/2) + 0) (textAboveMapY + mapOffsetY) $
        scale 0.2 0.2 $ color black $ text "You may draw your doodles here:"
      ]
  , drawEnemies (enemies gs)
  , drawPlacedTowers (towers gs)
  , renderWaveIndicator gs
  , renderCoins gs
  , renderTowerPrices
  , renderGameOver gs
  , renderTowerHP gs
  , renderPause (isPaused gs)
  , renderPauseButton
  , renderPauseMenu gs
  , renderShopMenu gs
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
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 90) $
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
  translate (fromIntegral windowWidth / 2 - 250)
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


renderGameOver :: GameState -> Picture
renderGameOver gs = case gameStatus gs of
  Victory -> centerMessage "🎉 Victory! You defended all waves!"
  Defeat  -> centerMessage "💀 Defeat! Enemies breached your tower!"
  _       -> Blank
  where
    centerMessage msg =
      translate (-200) 140 $
        scale 0.2 0.2 $
          color red $
            text msg


renderTowerHP :: GameState -> Picture
renderTowerHP gs =
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 60) $
    scale 0.15 0.15 $
    color black $
    if towerHP gs < 0 then text ("Tower HP: 0")
    else text ("Tower HP: " ++ show (towerHP gs))

renderPause :: Bool -> Picture
renderPause True =
  translate (-120) 0 $
    scale 0.3 0.3 $
      color red $
        text "⏸ PAUSED"
renderPause False = Blank


pauseButtonPos :: (Float, Float)
pauseButtonPos = (fromIntegral windowWidth / 2 - 60, fromIntegral windowHeight / 2 - 30)

pauseButtonSize :: (Float, Float)
pauseButtonSize = (100, 30)  -- width, height

resumeButtonPos, quitButtonPos :: (Float, Float)
resumeButtonPos = (0, 30)
quitButtonPos   = (0, -30)

menuButtonSize :: (Float, Float)
menuButtonSize = (180, 40)

renderPauseButton :: Picture
renderPauseButton =
  translate x y $
    Pictures
      [ color (greyN 0.8) $ rectangleSolid w h
      , color black $
          translate (-w/2 + 10) (-h/4) $
            scale 0.1 0.1 $
              text "Pause"
      ]
  where (x, y) = pauseButtonPos
        (w, h) = pauseButtonSize

renderPauseMenu :: GameState -> Picture
renderPauseMenu gs
  | not (showPauseMenu gs) = Blank
  | otherwise = translate 0 0 $
      Pictures
        [ color (makeColor 1 1 1 0.9) $ rectangleSolid 400 300
        , color black $
            translate (-140) 100 $
              scale 0.2 0.2 $
                text "Game Paused"
        , drawMenuButton resumeButtonPos "Resume"
        , drawMenuButton quitButtonPos   "Quit"
        ]
  where
    drawMenuButton :: (Float, Float) -> String -> Picture
    drawMenuButton (bx, by) label =
      translate bx by $
        Pictures
          [ color (light blue) $ rectangleSolid w h
          , color black $
              translate (-w/2 + 15) (-h/4) $
                scale 0.12 0.12 $
                  text label
          ]
      where (w, h) = menuButtonSize

renderIntro :: Float -> Picture
renderIntro t = Pictures $ introParts t

introParts :: Float -> [Picture]
introParts t =
  [ if t > 0 then fadeIn (t / 1.5) $ centerText 0 "Algorithms Defense" 0.3 else Blank
  , if t > 2 then fadeIn ((t - 2) / 1.5) $ centerText (-60) "Press SPACE to skip..." 0.15 else Blank
  ]

fadeIn :: Float -> Picture -> Picture
fadeIn alpha pic = Color (makeColor 0 0 0 (min 1 alpha)) pic

centerText :: Float -> String -> Float -> Picture
centerText y msg s = Translate (-200) y $ Scale s s $ Text msg

      

