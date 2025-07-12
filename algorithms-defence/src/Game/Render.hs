module Game.Render where

import Graphics.Gloss

import Game.Config
import Game.Shapes
import Game.Enemies
import Game.Logic ()
import Game.Types

render :: GameState -> Picture
render gs = Pictures
  [ -- Centered map group
    translate 0 mapOffsetY $ pictures
      [ paperTexture
      , mapBorder
      , lambdaPath
      , towerSpots
      , mainTower
      , translate (-(mapWidth/2) + 0) (textAboveMapY + mapOffsetY) $
      scale 0.2 0.2 $ color black $ text "You may draw your doodles here:"
      ]
      , drawEnemies (enemies gs)   -- enemies come from GameState now
  ]

drawEnemies :: [Enemy] -> Picture
drawEnemies = Pictures . map drawEnemy
