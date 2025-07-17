module Game.Shop (
  renderShopMenu,
  shopButtonRects,
  modificatorButtonRects,
  getModificatorAt
) where

import Graphics.Gloss
import Game.Shapes (drawArcherTowerRaw, drawCannonTowerRaw, drawSniperTowerRaw)
import Game.Types
import Game.Config

-- === Button Layout Constants ===

-- Tower button dimensions
buttonWidth, buttonHeight :: Float
buttonWidth  = 100
buttonHeight = 90

-- Base position below the map for button row
startX, startY :: Float
startX = -350
startY = - (mapHeight / 2) - 84  -- Slightly below the map

-- Spacing between tower buttons
buttonSpacing :: Float
buttonSpacing = 140

-- === Modificator Button Layout ===

modButtonWidth, modButtonHeight :: Float
modButtonWidth = 120
modButtonHeight = 80

-- Starting position for modificator buttons (right of towers)
modStartX, modStartY :: Float
modStartX = startX + 4 * buttonSpacing
modStartY = startY

-- === Render Shop Menu ===

-- Draws the entire shop menu including tower and modificator buttons
renderShopMenu :: GameState -> Picture
renderShopMenu gs = 
  translate 0 startY $
    Pictures $
      zipWith (drawTowerButton (selectedTower gs)) [0..] towerTypes
      ++ zipWith (drawModificatorButton (selectedModificator gs)) [0..] modificatorTypes

-- Ordered list of tower types for buttons
towerTypes :: [TowerType]
towerTypes = [Archer, Cannon, Sniper]

-- Ordered list of modificator types for buttons
modificatorTypes :: [Modificator]
modificatorTypes = [Map, Pop]

-- === Button Click Areas ===

-- Defines clickable areas for tower shop buttons
shopButtonRects :: [(TowerType, (Float, Float), (Float, Float))]
shopButtonRects =
  [ (ttype, (startX + fromIntegral i * buttonSpacing, startY), (buttonWidth, buttonHeight))
  | (i, ttype) <- zip [0..] towerTypes
  ]

-- Defines clickable areas for modificator buttons
modificatorButtonRects :: [(Modificator, (Float, Float), (Float, Float))]
modificatorButtonRects =
  [ (mod, (modStartX + fromIntegral i * buttonSpacing, modStartY), (modButtonWidth, modButtonHeight))
  | (i, mod) <- zip [0..] modificatorTypes
  ]

-- Checks if mouse coordinates fall within any modificator button
getModificatorAt :: Float -> Float -> Maybe Modificator
getModificatorAt mx my =
  case filter isInside modificatorButtonRects of
    ((mod, _, _):_) -> Just mod
    _               -> Nothing
  where
    isInside (_, (x, y), (w, h)) =
      abs (mx - x) <= w/2 && abs (my - y) <= h/2

-- === Drawing Buttons ===

-- Draws one tower button with type, cost, stats, and icon
drawTowerButton :: TowerType -> Int -> TowerType -> Picture
drawTowerButton selected idx ttype =
  translate (startX + fromIntegral idx * buttonSpacing) 0 $
    Pictures
      [ color (if selected == ttype then light green else greyN 0.7) $
          rectangleSolid buttonWidth buttonHeight

      , color (if selected == ttype then red else black) $
          rectangleWire (buttonWidth + 4) (buttonHeight + 4)

      , translate 30 (buttonHeight / 2 - 20) $ drawMiniTower ttype

      , labelText (-buttonWidth / 2 + 10) 35 0.1 (show ttype)
      , labelText (-buttonWidth / 2 + 10) 16 0.08 ("Cost: " ++ show (towerCost ttype))
      , labelText (-buttonWidth / 2 + 10) 0 0.08 ("Dmg: " ++ show (towerDamageFor ttype))
      , labelText (-buttonWidth / 2 + 10) (-15) 0.08 ("CD: " ++ show (cooldownFor ttype) ++ "s")

      -- Bottom label + coin icon
      , translate (-buttonWidth / 2 + 10) (-40) $
          Pictures
            [ scale 0.12 0.12 $ color black $ text ("Price: " ++ show (towerCost ttype))
            , translate 82 6 $
                color (makeColorI 255 215 0 255) $ thickCircle 5 3
            ]
      ]

-- Draws a modificator button with cost and effect description
drawModificatorButton :: Maybe Modificator -> Int -> Modificator -> Picture
drawModificatorButton selected idx mod =
  translate (modStartX + fromIntegral idx * buttonSpacing) 0 $
    Pictures
      [ color (greyN 0.6) $
          rectangleSolid modButtonWidth modButtonHeight

      , color black $
          rectangleWire (modButtonWidth + 3) (modButtonHeight + 3)

      , labelText (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 15) 0.1 (show mod)
      , labelText (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 35) 0.1 ("Cost: " ++ show (modificatorCost mod))
      , labelText (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 55) 0.1 (modDescription mod)

      , color red $
          translate (-modButtonWidth / 2 + 2) (-modButtonHeight / 2 + 5) $
            scale 0.1 0.1 $ text "Click tower first!"

      -- Coin icon next to cost
      , translate (-modButtonWidth / 2 + 56) (modButtonHeight / 2 - 30) $
          Pictures
            [ translate (textWidth ("Cost: " ++ show (modificatorCost mod)) * 0.1 + 10) 0 $
                color gold $ thickCircle 4 2
            ]
      ]
  where
    textWidth txt = fromIntegral (length txt) * 6
    gold = makeColorI 255 215 0 255

-- Simple text drawing utility
labelText :: Float -> Float -> Float -> String -> Picture
labelText x y s txt =
  color black $ translate x y $ scale s s $ text txt

-- Description of each modificator
modDescription :: Modificator -> String
modDescription Map = "Area damage"
modDescription Pop = "Instant kill"
modDescription GarbageCollector = "Cleans garbage"

-- Draws a mini version of the tower image with a border
drawMiniTower :: TowerType -> Picture
drawMiniTower ttype =
  Pictures
    [ scale 0.4 0.4 $ towerPic
    , color (greyN 0.3) $ rectangleWire 40 40
    ]
  where
    towerPic = case ttype of
      Archer -> drawArcherTowerRaw
      Cannon -> drawCannonTowerRaw
      Sniper -> drawSniperTowerRaw
