module Game.Shapes where

import Graphics.Gloss
import Game.Config

drawMainTower :: (Int, Int) -> Float -> Picture
drawMainTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $ 
      Pictures
        [ -- Stem (pillar)
          translate 0 (-cellSize * 0.2) $
            color (makeColorI 70 70 160 255) $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.8)
          
        , -- Circle dome on top
          translate 0 (cellSize * 0.3) $
            color (makeColorI 0 120 220 255) $
            thickCircle (cellSize * 0.2) 5
        ]

arrowhead :: Picture
arrowhead =
  color black $
    polygon
      [ (0, 0)
      , (-cellSize * 0.05, cellSize * 0.03)
      , (-cellSize * 0.05, -cellSize * 0.03)
      ]


drawArcherTower :: (Int, Int) -> Float -> Picture
drawArcherTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $
      Pictures
        [ -- Tower base
          color (makeColorI 150 75 0 255) $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.7)

        -- Full Bow (rotated as one shape)
        , translate (cellSize * 0.05) (cellSize * 0.5) $  -- position the whole bow
            rotate (-15) $  -- 🔁 rotate whole bow group
            Pictures
                [ -- Bow (semicircle)
                color black $
                    arc 270 90 (cellSize * 0.25)

                -- Arrow
                , color black $
                    line [(-cellSize * 0.25, 0), (cellSize * 0.4, 0)]

                -- Vertical string
                , color black $
                    line [(0, -cellSize * 0.25), (0, cellSize * 0.25)]

                -- Arrowhead at end
                , translate (cellSize * 0.4) 0 $ 
                    arrowhead
            ]
        ]
  where
    w = cellSize * 0.2
    h = cellSize * 0.3



drawCannonTower :: (Int, Int) -> Float -> Picture
drawCannonTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $
      Pictures
        [ color (greyN 0.3) $
            rectangleSolid (cellSize * 0.4) (cellSize * 0.5)
        , translate (cellSize * 0.35) 0 $
            color black $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.1)
        ]


drawSniperTower :: (Int, Int) -> Float -> Picture
drawSniperTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $
      Pictures
        [ -- Tower body
          color (makeColorI 80 80 140 255) $
            rectangleSolid (cellSize * 0.2) (cellSize * 1)

          -- Long sniper barrel (line)
        , translate (cellSize * 0) (cellSize * 0.5) $
            Pictures
                [
                color black $
                    rectangleSolid (cellSize * 0.7) (cellSize * 0.05)

                , color black $
                    arc 0 180 (cellSize * 0.25)

                , translate (cellSize * 0) (cellSize * 0.12) $
                    color orange $
                    circleSolid 1.9
                ]
        ]



drawTopGate = translate x y $ color red $ ThickArc 0 180 (cellSize * 2) 5
  where
    x = colToX (gridWidth `div` 2)
    y = rowToY (gridHeight - 1) + cellSize / 2


drawRightGate = translate x y $ Pictures
  [ translate (-5) 0 $ color red $ rectangleSolid 4 cellSize
  , translate ( 5) 0 $ color red $ rectangleSolid 4 cellSize
  ]
  where
    x = colToX (gridWidth - 1) + cellSize
    y = rowToY (gridHeight `div` 2)

