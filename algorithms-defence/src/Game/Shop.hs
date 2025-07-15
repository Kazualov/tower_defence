module Game.Shop (
  renderShopMenu,
  shopButtonRects
) where

import Graphics.Gloss
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
buttonSpacing = 110  -- space between buttons horizontally

renderShopMenu :: GameState -> Picture
renderShopMenu gs = 
  translate 0 startY $
    Pictures $ zipWith (drawTowerButton (selectedTower gs)) [0..] [Archer, Cannon, Sniper]

-- Tower buttons in left-to-right order
towerTypes :: [TowerType]
towerTypes = [Archer, Cannon, Sniper]

-- Rectangle centers for each shop button (used for click detection)
shopButtonRects :: [(TowerType, (Float, Float), (Float, Float))]
shopButtonRects =
  [ (ttype, (startX + fromIntegral i * buttonSpacing, startY), (buttonWidth, buttonHeight))
  | (i, ttype) <- zip [0..] towerTypes
  ]

drawTowerButton :: TowerType -> Int -> TowerType -> Picture
drawTowerButton selected idx ttype =
  translate (startX + fromIntegral idx * buttonSpacing) 0 $
    Pictures
      [ -- background
        color (if selected == ttype then light green else greyN 0.7) $
          rectangleSolid buttonWidth buttonHeight
      , color black $
          translate (-buttonWidth / 2 + 10) (buttonHeight / 2 - 20) $
            scale 0.1 0.1 $ text (show ttype)
      , color black $
          translate (-buttonWidth / 2 + 10) (buttonHeight / 2 - 40) $
            scale 0.08 0.08 $ text ("Cost: " ++ show (towerCost ttype))
      , color black $
          translate (-buttonWidth / 2 + 10) (buttonHeight / 2 - 60) $
            scale 0.08 0.08 $ text ("Dmg: " ++ show (towerDamageFor ttype))
      , color black $
          translate (-buttonWidth / 2 + 10) (buttonHeight / 2 - 80) $
            scale 0.08 0.08 $ text ("CD: " ++ show (cooldownFor ttype) ++ "s")
      ]
