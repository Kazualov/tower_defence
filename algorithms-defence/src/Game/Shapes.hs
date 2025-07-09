module Game.Shapes where

import Graphics.Gloss
import Game.Config

-- White background rectangle
paperTexture :: Picture
paperTexture = color white $ rectangleSolid mapWidth mapHeight

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
  , curveLine [(-50, -10), (0, 30), (80, 70), (mapWidth/2 - 30, 90)]     -- top branch
  , curveLine [(0, -20), (0, 10), (80, 40), (mapWidth/2 - 30, 60)]  -- top branch
  , curveLine [(0, -20), (0, -20), (80, -40), (mapWidth/2 - 30, -60)]     -- top branch
  , curveLine [(-50, -40), (0, -35), (80, -70), (mapWidth/2 - 30, -90)]  -- top branch
  ]

-- Approximates a curved line with interpolation
curveLine :: [Point] -> Picture
curveLine pts = line $ bezierInterp pts 40

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
