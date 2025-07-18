module Game.Shapes where

import Graphics.Gloss
import Game.Config

-- | A white rectangle representing the background (like a sheet of paper).
paperTexture :: Picture
paperTexture = color white $ rectangleSolid mapWidth mapHeight

-- | Draw the main tower at a given grid position with a scale.
drawMainTower :: (Int, Int) -> Float -> Picture
drawMainTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $ 
      Pictures
        [ -- Tower base (stem)
          translate 0 (-cellSize * 0.2) $
            color (makeColorI 70 70 160 255) $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.8)
          
        , -- Dome on top of tower
          translate 0 (cellSize * 0.3) $
            color (makeColorI 0 120 220 255) $
            thickCircle (cellSize * 0.2) 5
        ]

-- | A small black triangle used as the arrowhead.
arrowhead :: Picture
arrowhead =
  color black $
    polygon
      [ (0, 0)
      , (-cellSize * 0.05, cellSize * 0.03)
      , (-cellSize * 0.05, -cellSize * 0.03)
      ]

-- | Black rectangle outline representing the game map border.
mapBorder :: Picture
mapBorder = color black $ rectangleWire mapWidth mapHeight

-- | A small static main tower drawn on the map as the letter "A".
mainTower :: Picture
mainTower = translate towerX towerY $ scale 1.5 1.5 $ color black $ pictures
  [ line [(-10, -20), (0, 20), (10, -20)]             -- A shape
  , translate 0 0 $ rectangleSolid 12 1              -- crossbar of A
  ]
  where
    towerX = -mapWidth / 2 + 40
    towerY = -20
-- | A stylized lambda-shaped path made of multiple thick lines and curves.
lambdaPath :: Picture
lambdaPath = pictures
  [ polygonalThickLine 4 [(-mapWidth/2 + 60, -10), (-50, -10)]
  , polygonalThickLine 4 [(-mapWidth/2 + 60, -40), (-50, -40)]
  , polygonalThickLine 4 (bezierInterp [(-50, -10), (0, 20), (80, 70), (mapWidth/2 - 30, 90), (mapWidth/2 - 2, 100)] 10)
  , polygonalThickLine 4 (bezierInterp [(0, -20), (0, 0), (80, 40), (mapWidth/2 - 30, 60), (mapWidth/2 - 2, 60)] 10)
  , polygonalThickLine 4 (bezierInterp [(0, -20), (0, -20), (80, -40), (mapWidth/2 - 30, -60), (mapWidth/2 - 2, -40)] 10)
  , polygonalThickLine 4 (bezierInterp [(-50, -40), (0, -35), (80, -70), (mapWidth/2 - 30, -90), (mapWidth/2 - 2, -100)] 10)
  ]

-- | Draws a thick polyline between given points using black rectangles.
polygonalThickLine :: Float -> [Point] -> Picture
polygonalThickLine thickness pts = pictures $ zipWith quad pts (tail pts)
  where
    halfT = thickness / 2
    -- Create rectangle between each pair of points
    quad (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
          len = sqrt (dx * dx + dy * dy)
          (nx, ny) = (-dy / len, dx / len)
          p1 = (x1 + nx * halfT, y1 + ny * halfT)
          p2 = (x1 - nx * halfT, y1 - ny * halfT)
          p3 = (x2 - nx * halfT, y2 - ny * halfT)
          p4 = (x2 + nx * halfT, y2 + ny * halfT)
      in color black $ polygon [p1, p2, p3, p4]

-- | Generate points along a bezier curve for smoother path shapes.
bezierInterp :: [Point] -> Int -> [Point]
bezierInterp pts n = [ bezierPoint pts (fromIntegral i / fromIntegral n) | i <- [0..n] ]

-- | Recursively interpolate points using De Casteljau's algorithm.
bezierPoint :: [Point] -> Float -> Point
bezierPoint [p] _ = p
bezierPoint ps t = bezierPoint (zipWith (lerp t) ps (tail ps)) t

-- | Linear interpolation between two points.
lerp :: Float -> Point -> Point -> Point
lerp t (x1,y1) (x2,y2) = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)

-- | Draws 'X' marks at predefined tower placement spots.
towerSpots :: Picture
towerSpots = color black $ pictures $ map drawX positions
  where
    drawX (x, y) = translate x y $ scale 0.15 0.15 $ text "x"
    positions =
      [ (-100, 20), (-100, -30)
      , (-30, 30),  (-30, -50)
      , (40, 60),   (40, -80)
      , (100, 75),  (100, -90)
      , (150, 50),  (150, -60)
      , (-10, -5)
      ]

-----------------------------------------------------
-- Raw tower pictures used for mini-icons in shop
-----------------------------------------------------

-- | Small preview of the archer tower.
drawArcherTowerRaw :: Picture
drawArcherTowerRaw = scale 2 2 $
  Pictures
    [ color (makeColorI 150 75 0 255) $
        rectangleSolid (cellSize * 0.3) (cellSize * 0.7)

    , translate (cellSize * 0.05) (cellSize * 0.5) $
        rotate (-15) $
        Pictures
          [ color black $ arc 270 90 (cellSize * 0.25)
          , color black $ line [(-cellSize * 0.25, 0), (cellSize * 0.4, 0)]
          , color black $ line [(0, -cellSize * 0.25), (0, cellSize * 0.25)]
          , translate (cellSize * 0.4) 0 $ arrowhead
          ]
    ]

-- | Small preview of the cannon tower.
drawCannonTowerRaw :: Picture
drawCannonTowerRaw = scale 2 2 $
  Pictures
    [ color (greyN 0.3) $
        rectangleSolid (cellSize * 0.4) (cellSize * 0.5)
    , translate (cellSize * 0.35) 0 $
        color black $
          rectangleSolid (cellSize * 0.3) (cellSize * 0.1)
    ]

-- | Small preview of the sniper tower.
drawSniperTowerRaw :: Picture
drawSniperTowerRaw = scale 2 2 $
  Pictures
    [ color (makeColorI 80 80 140 255) $
        rectangleSolid (cellSize * 0.2) (cellSize * 1)
    , translate (cellSize * 0) (cellSize * 0.5) $
        Pictures
          [ color black $ rectangleSolid (cellSize * 0.7) (cellSize * 0.05)
          , color black $ arc 0 180 (cellSize * 0.25)
          , translate (cellSize * 0) (cellSize * 0.12) $
              color orange $ circleSolid 1.9
          ]
    ]

-----------------------------------------------------
-- Full tower drawings for actual map placement
-----------------------------------------------------

-- | Draw full-size archer tower at grid location with scale.
drawArcherTower :: (Int, Int) -> Float -> Picture
drawArcherTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $
      Pictures
        [ color (makeColorI 150 75 0 255) $
            rectangleSolid (cellSize * 0.3) (cellSize * 0.7)
        , translate (cellSize * 0.05) (cellSize * 0.5) $
            rotate (-15) $
              Pictures
                [ color black $ arc 270 90 (cellSize * 0.25)
                , color black $ line [(-cellSize * 0.25, 0), (cellSize * 0.4, 0)]
                , color black $ line [(0, -cellSize * 0.25), (0, cellSize * 0.25)]
                , translate (cellSize * 0.4) 0 $ arrowhead
                ]
        ]

-- | Draw full-size cannon tower at grid location with scale.
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

-- | Draw full-size sniper tower at grid location with scale.
drawSniperTower :: (Int, Int) -> Float -> Picture
drawSniperTower (col, row) s =
  translate (colToX col) (rowToY row) $
    scale s s $
      Pictures
        [ color (makeColorI 80 80 140 255) $
            rectangleSolid (cellSize * 0.2) (cellSize * 1)
        , translate (cellSize * 0) (cellSize * 0.5) $
            Pictures
              [ color black $ rectangleSolid (cellSize * 0.7) (cellSize * 0.05)
              , color black $ arc 0 180 (cellSize * 0.25)
              , translate (cellSize * 0) (cellSize * 0.12) $
                  color orange $ circleSolid 1.9
              ]
        ]
