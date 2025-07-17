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

-- Button layout
buttonWidth, buttonHeight :: Float
buttonWidth  = 100
buttonHeight = 90

-- Positioning below map
startX, startY :: Float
startX = -350  -- starting X for the first button
startY = - (mapHeight / 2) - 84 --below the map

-- Spacing
buttonSpacing :: Float
buttonSpacing = 140  -- space between buttons horizontally

-- Modificator button layout
modButtonWidth, modButtonHeight :: Float
modButtonWidth = 120
modButtonHeight = 80

modStartX, modStartY :: Float
modStartX = startX + 4 * buttonSpacing  -- After tower buttons
modStartY = startY

renderShopMenu :: GameState -> Picture
renderShopMenu gs = 
  translate 0 startY $
    Pictures $ 
      zipWith (drawTowerButton (selectedTower gs)) [0..] [Archer, Cannon, Sniper]
      ++ zipWith (drawModificatorButton (selectedModificator gs)) [0..] [Map, Pop]

-- Tower buttons in left-to-right order
towerTypes :: [TowerType]
towerTypes = [Archer, Cannon, Sniper]

-- Modificator buttons
modificatorTypes :: [Modificator]
modificatorTypes = [Map, Pop]

-- Rectangle centers for each shop button (used for click detection)
shopButtonRects :: [(TowerType, (Float, Float), (Float, Float))]
shopButtonRects =
  [ (ttype, (startX + fromIntegral i * buttonSpacing, startY), (buttonWidth, buttonHeight))
  | (i, ttype) <- zip [0..] towerTypes
  ]

-- Rectangle centers for each modificator button
modificatorButtonRects :: [(Modificator, (Float, Float), (Float, Float))]
modificatorButtonRects =
  [ (mod, (modStartX + fromIntegral i * buttonSpacing, modStartY), (modButtonWidth, modButtonHeight))
  | (i, mod) <- zip [0..] modificatorTypes
  ]

-- Get modificator at position
getModificatorAt :: Float -> Float -> Maybe Modificator
getModificatorAt mx my =
  case filter isInside modificatorButtonRects of
    ((mod, _, _):_) -> Just mod
    _               -> Nothing
  where
    isInside (_, (x, y), (w, h)) =
      abs (mx - x) <= w/2 && abs (my - y) <= h/2

drawTowerButton :: TowerType -> Int -> TowerType -> Picture
drawTowerButton selected idx ttype =
  translate (startX + fromIntegral idx * buttonSpacing) 0 $
    Pictures
      [ -- background
        color (if selected == ttype then light green else greyN 0.7) $
          rectangleSolid buttonWidth buttonHeight

      -- full border of button
      , color (if selected == ttype then red else black) $
          rectangleWire (buttonWidth + 4) (buttonHeight + 4)

      -- tower image + its frame
      , translate 30 (buttonHeight / 2 - 20) $ drawMiniTower ttype

      -- tower type label
      , color black $
          translate (-buttonWidth / 2 + 10) 35 $
            scale 0.1 0.1 $ text (show ttype)

      -- tower cost
      , color black $
          translate (-buttonWidth / 2 + 10) (16) $
            scale 0.08 0.08 $ text ("Cost: " ++ show (towerCost ttype))

      -- damage
      , color black $
          translate (-buttonWidth / 2 + 10) (0) $
            scale 0.08 0.08 $ text ("Dmg: " ++ show (towerDamageFor ttype))

      -- cooldown
      , color black $
          translate (-buttonWidth / 2 + 10) (-15) $
            scale 0.08 0.08 $ text ("CD: " ++ show (cooldownFor ttype) ++ "s")
      
      , translate (-buttonWidth / 2 + 10) (-40) $
          Pictures
            [ scale 0.12 0.12 $ color black $ text ("Price: " ++ show (towerCost ttype))
            -- Coin icon
            , translate 82 (6) $  -- Adjust position relative to text
                color (makeColorI 255 215 0 255) $  -- gold color
                  thickCircle 5 3  -- Smaller coin icon
            ]
      ]

      

drawModificatorButton :: Maybe Modificator -> Int -> Modificator -> Picture
drawModificatorButton selectedTower idx mod =
  translate (modStartX + fromIntegral idx * buttonSpacing) 0 $
    Pictures
      [ -- background
        color (greyN 0.6) $
          rectangleSolid modButtonWidth modButtonHeight

      -- border
      , color black $
          rectangleWire (modButtonWidth + 3) (modButtonHeight + 3)

      -- mod name
      , color black $
          translate (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 15) $
            scale 0.1 0.1 $ text (show mod)

      -- cost
      , color black $
          translate (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 35) $
            scale 0.1 0.1 $ text ("Cost: " ++ show (modificatorCost mod))

      -- effect description
      , color black $
          translate (-modButtonWidth / 2 + 2) (modButtonHeight / 2 - 55) $
            scale 0.1 0.1 $ text (modDescription mod)

      -- Instructions
      , color red $
          translate (-modButtonWidth / 2 + 2) (-modButtonHeight / 2 + 5) $
            scale 0.1 0.1 $ text "Click tower first!"
      , translate (-modButtonWidth / 2 + 56) (modButtonHeight / 2 - 30) $
        Pictures
          [translate (textWidth ("Cost: " ++ show (modificatorCost mod)) * 0.1 + 10) 0 $
              color gold $ thickCircle 4 2
          ]
      ]
  where
    textWidth txt = fromIntegral (length txt) * 6
    gold = makeColorI 255 215 0 255

    
modDescription :: Modificator -> String
modDescription Map = "Area damage"
modDescription Pop = "Instant kill"
modDescription GarbageCollector = "Cleans garbage"

-- Draws the small tower image + frame
drawMiniTower :: TowerType -> Picture
drawMiniTower ttype =
  Pictures
    [ scale 0.4 0.4 $ towerPic  -- Smaller version of the real tower
    , color (greyN 0.3) $ rectangleWire 40 40  -- frame around the picture
    ]
  where
    towerPic = case ttype of
      Archer -> drawArcherTowerRaw
      Cannon -> drawCannonTowerRaw
      Sniper -> drawSniperTowerRaw