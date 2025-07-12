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

lambdaPath :: Picture
lambdaPath = pictures
  [ polygonalThickLine 4 [(-mapWidth/2 + 60, -10), (-50, -10)]
  , polygonalThickLine 4 [(-mapWidth/2 + 60, -40), (-50, -40)]
  , polygonalThickLine 4 (bezierInterp [(-50, -10), (0, 20), (80, 70), (mapWidth/2 - 30, 90), (mapWidth/2 - 2, 100)] 10)
  , polygonalThickLine 4 (bezierInterp [(0, -20), (0, 0), (80, 40), (mapWidth/2 - 30, 60), (mapWidth/2 - 2, 60)] 10)
  , polygonalThickLine 4 (bezierInterp [(0, -20), (0, -20), (80, -40), (mapWidth/2 - 30, -60), (mapWidth/2 - 2, -40)] 10)
  , polygonalThickLine 4 (bezierInterp [(-50, -40), (0, -35), (80, -70), (mapWidth/2 - 30, -90), (mapWidth/2 - 2, -100)] 10)
  ]

-- Построение толстой линии (ленты) как полигона
polygonalThickLine :: Float -> [Point] -> Picture
polygonalThickLine thickness pts = pictures $ zipWith quad pts (tail pts)
  where
    halfT = thickness / 2

    -- Для каждой пары точек делаем прямоугольник между ними
    quad (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
          len = sqrt (dx * dx + dy * dy)
          -- нормализованный перпендикулярный вектор
          (nx, ny) = (-dy / len, dx / len)
          -- верхние и нижние точки ленты
          p1 = (x1 + nx * halfT, y1 + ny * halfT)
          p2 = (x1 - nx * halfT, y1 - ny * halfT)
          p3 = (x2 - nx * halfT, y2 - ny * halfT)
          p4 = (x2 + nx * halfT, y2 + ny * halfT)
      in color black $ polygon [p1, p2, p3, p4]

-- Quadratic Bezier interpolation
bezierInterp :: [Point] -> Int -> [Point]
bezierInterp pts n = [ bezierPoint pts (fromIntegral i / fromIntegral n) | i <- [0..n] ]

bezierPoint :: [Point] -> Float -> Point
bezierPoint [p] _ = p
bezierPoint ps t = bezierPoint (zipWith (lerp t) ps (tail ps)) t

lerp :: Float -> Point -> Point -> Point
lerp t (x1,y1) (x2,y2) = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
