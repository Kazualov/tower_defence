module Game.Shapes where

import Graphics.Gloss
import Game.Config


-- White background rectangle
paperTexture :: Picture
paperTexture = color white $ rectangleSolid mapWidth mapHeight

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

-- Black border
mapBorder :: Picture
mapBorder = color black $ rectangleWire mapWidth mapHeight

-- Main tower: circle (head) + rectangle (base)
mainTower :: Picture
mainTower = translate towerX towerY $ pictures
  [ color black $ translate 0 20 $ circleSolid 15
  , color black $ rectangleSolid 10 30
  ]
  where
    towerX = -mapWidth / 2 + 40
    towerY = -20

-- Hand-drawn-style path (left to forked Y)
lambdaPath :: Picture
lambdaPath = color black $ pictures
  [ line [(-mapWidth/2 + 60, -10), (-50, -10)]                             -- stem
  , line [(-mapWidth/2 + 60, -40), (-50, -40)]                             -- stem
  , curveLine [(-50, -10), (0, 30), (80, 70), (mapWidth/2 - 30, 90)]       -- top branch
  , curveLine [(0, -20), (0, 10), (80, 40), (mapWidth/2 - 30, 60)]         -- top branch
  , curveLine [(0, -20), (0, -20), (80, -40), (mapWidth/2 - 30, -60)]      -- bottom branch
  , curveLine [(-50, -40), (0, -35), (80, -70), (mapWidth/2 - 30, -90)]    -- bottom branch
  ]

-- Approximates a curved line with interpolation
curveLine :: [Point] -> Picture
curveLine pts = line $ bezierInterp pts 100

-- Quadratic Bezier interpolation
bezierInterp :: [Point] -> Int -> [Point]
bezierInterp pts n = [ bezier pts (t / fromIntegral n) | t <- [0..fromIntegral n] ]
  where
    bezier [p] _ = p
    bezier ps t = bezier (zipWith (interp t) ps (tail ps)) t
    interp t (x1,y1) (x2,y2) = ((1 - t)*x1 + t*x2, (1 - t)*y1 + t*y2)

-- Tower placement spots: Xs near the path
towerSpots :: Picture
towerSpots = color black $ pictures $ map drawX positions
  where
    drawX (x, y) = translate x y $ scale 0.15 0.15 $ text "x"
    positions =
      [ (-100, 20), (-100, -30)
      , (-30, 30),  (-30, -50)
      , (40, 60),   (40, -80)
      , (100, 75),  (100, -90)
      , (150, 50),  (150, -60),
        (-10, -5)
      ]

drawArcherTowerRaw :: Picture
drawArcherTowerRaw = scale 2 2 $
  Pictures
    [ color (makeColorI 150 75 0 255) $
        rectangleSolid (cellSize * 0.3) (cellSize * 0.7)

    , translate (cellSize * 0.05) (cellSize * 0.5) $
        rotate (-15) $
        Pictures
          [ color black $
              arc 270 90 (cellSize * 0.25)
          , color black $
              line [(-cellSize * 0.25, 0), (cellSize * 0.4, 0)]
          , color black $
              line [(0, -cellSize * 0.25), (0, cellSize * 0.25)]
          , translate (cellSize * 0.4) 0 $
              arrowhead
          ]
    ]


drawCannonTowerRaw :: Picture
drawCannonTowerRaw = scale 2 2 $
  Pictures
        [ color (greyN 0.3) $
            rectangleSolid (cellSize * 0.4) (cellSize * 0.5)
        , translate (cellSize * 0.35) 0 $
            color black $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.1)
        ]

drawSniperTowerRaw :: Picture
drawSniperTowerRaw = scale 2 2 $
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