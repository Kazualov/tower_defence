module Game.Render where

import Graphics.Gloss

import Game.Shop (renderShopMenu)
import Game.Config
import Game.Shapes hiding (towerSpots)
import Game.Enemies
import Game.Logic ()
import Game.Types

-- Main render function: delegates rendering based on game status
render :: GameState -> Picture
render gs = case gameStatus gs of
  Intro idx _ -> renderIntro gs    -- Render intro screen
  Playing     -> renderGame gs     -- Render main game
  Victory     -> renderGameOver gs -- Render victory message
  Defeat      -> renderGameOver gs -- Render defeat message

-- Render the main game scene
renderGame :: GameState -> Picture
renderGame gs = Pictures
  [ translate 0 mapOffsetY $ pictures
      [ mapBorder                             -- Border around the map
      , lambdaPath                            -- Path the enemies follow
      , drawTowerSpots (towerSpots gs)        -- Tower placement markers
      , mainTower                             -- Central tower image
      , translate (-(mapWidth/2) + 0) (textAboveMapY + mapOffsetY) $
          scale 0.2 0.2 $ color black $ text "You may draw your doodles here:"
      ]
  , drawEnemies (enemies gs)                 -- All enemies on screen
  , drawPlacedTowers (towers gs)             -- All placed towers
  , renderWaveIndicator gs                   -- Current wave info
  , renderCoins gs                           -- Coins display
  , renderGameOver gs                        -- Game over messages (Victory/Defeat)
  , renderTowerHP gs                         -- Central tower HP
  , renderPause (isPaused gs)                -- Pause indicator
  , renderPauseButton                        -- Pause button
  , renderPauseMenu gs                       -- Pause menu (if open)
  , renderShopMenu gs                        -- Shop UI
  , renderSelectedTowerIndicator gs          -- Highlight selected tower for upgrade/mod
  ]

-- Render all enemies
drawEnemies :: [Enemy] -> Picture 
drawEnemies = Pictures . map drawEnemy

-- Render all towers
drawPlacedTowers :: [Tower] -> Picture
drawPlacedTowers = pictures . map drawTowerAtScreen

-- Draw a single tower at its screen position
drawTowerAtScreen :: Tower -> Picture
drawTowerAtScreen tower@(Tower ttype pos _ _ mod) =
  translate x y $
    Pictures
      [ case ttype of                -- Choose image based on tower type
          Archer -> drawArcherTowerRaw
          Cannon -> drawCannonTowerRaw
          Sniper -> drawSniperTowerRaw
      , drawModificatorIndicator mod -- Draw optional upgrade/mod symbol
      ]
  where (x, y) = pos

-- Draw visual indicator for a tower modificator (if any)
drawModificatorIndicator :: Maybe Modificator -> Picture
drawModificatorIndicator Nothing = Blank
drawModificatorIndicator (Just mod) =
  translate 15 15 $
    Pictures
      [ color (modColor mod) $ thickCircle 8 3
      , color black $
          translate (-3) (-3) $
            scale 0.08 0.08 $ text (modSymbol mod)
      ]

-- Assign color to each modificator
modColor :: Modificator -> Color
modColor Map              = yellow
modColor Pop              = red
modColor GarbageCollector = green

-- Assign short label to each modificator
modSymbol :: Modificator -> String
modSymbol Map              = "M"
modSymbol Pop              = "P"
modSymbol GarbageCollector = "G"

-- Highlight selected tower with an orange circle
renderSelectedTowerIndicator :: GameState -> Picture
renderSelectedTowerIndicator gs =
  case selectedTowerForMod gs of
    Nothing -> Blank
    Just tower ->
      let (x, y) = towerPos tower
      in translate x y $
           color orange $
             thickCircle 30 5

-- Convert screen X coordinate to column index
xToCol :: Float -> Int
xToCol x = round (x / cellSize)

-- Convert screen Y coordinate to row index
yToRow :: Float -> Int
yToRow y = round (y / cellSize)

-- Draw all tower placement spots as small "x" markers
drawTowerSpots :: [TowerSpot] -> Picture
drawTowerSpots = color black . pictures . map drawX
  where
    drawX (x, y) = translate x y $ scale 0.15 0.15 $ text "x"

-- Show wave indicator in top-left corner
renderWaveIndicator :: GameState -> Picture
renderWaveIndicator gs =
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 30)
    $ scale 0.15 0.15
    $ color black
    $ text ("Wave: " ++ show (currentWave gs + 1))

-- Show coin count with coin icon in top-left corner
renderCoins :: GameState -> Picture
renderCoins gs =
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 90) $
    Pictures
      [ translate 113 8 $
          color (makeColorI 255 215 0 255) $
            thickCircle 7 5  -- Coin icon
      , translate 0 0 $
          scale 0.15 0.15 $
            color black $
              text ("Coins: " ++ show (coins gs))
      ]

-- Show game over message (Victory/Defeat)
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

-- Display central tower HP
renderTowerHP :: GameState -> Picture
renderTowerHP gs =
  translate (-fromIntegral windowWidth / 2 + 10)
            (fromIntegral windowHeight / 2 - 60) $
    scale 0.15 0.15 $
    color black $
    text $ "Tower HP: " ++ show (max 0 (towerHP gs))

-- Display red "⏸ PAUSED" text when paused
renderPause :: Bool -> Picture
renderPause True =
  translate (-120) 0 $
    scale 0.3 0.3 $
      color red $
        text "⏸ PAUSED"
renderPause False = Blank

-- Coordinates and size of pause button
pauseButtonPos :: (Float, Float)
pauseButtonPos = (fromIntegral windowWidth / 2 - 60, fromIntegral windowHeight / 2 - 30)

pauseButtonSize :: (Float, Float)
pauseButtonSize = (100, 30)

-- Coordinates for pause menu buttons
resumeButtonPos, quitButtonPos :: (Float, Float)
resumeButtonPos = (0, 30)
quitButtonPos   = (0, -30)

-- Size of buttons in pause menu
menuButtonSize :: (Float, Float)
menuButtonSize = (180, 40)

-- Draw pause button in upper-right corner
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

-- Render pause menu overlay with "Resume" and "Quit" buttons
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

-- Render intro screen (first-time launch story slides or tutorial)
renderIntro :: GameState -> Picture
renderIntro gs = case gameStatus gs of
  Intro idx _ ->
    let image = if idx < length (introImages gs)
                then introImages gs !! idx
                else Blank
    in Pictures [ scale 0.5 0.5 $ translate (-20) (-10) image ]
  _ -> Blank

-- Apply transparency to a picture
fadeIn :: Float -> Picture -> Picture
fadeIn alpha pic = Color (makeColor 0 0 0 (min 1 alpha)) pic

-- Helper to center and scale text
centerText :: Float -> String -> Float -> Picture
centerText y msg s = Translate (-200) y $ Scale s s $ Text msg
