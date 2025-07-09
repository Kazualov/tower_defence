module Game.Render where

import Graphics.Gloss
import Game.Config
import Game.Shapes
import Game.Types

render :: GameState -> Picture
render _ = pictures
  [ -- Centered map group
    translate 0 mapOffsetY $ pictures
      [ paperTexture
      , mapBorder
      , lambdaPath
      , towerSpots
      , mainTower
      ]

  -- Text ~5px above top edge of map
  , translate (-mapWidth/2 + 0) (textAboveMapY + mapOffsetY) $
      scale 0.2 0.2 $ color black $ text "You may draw your doodles here:"
  ]
