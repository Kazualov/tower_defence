module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (find)

import Game.Types
import Game.Shop (shopButtonRects)
import Game.Render
import Game.Logic
import Game.Config
import Game.Enemies


-- Main entry point
main :: IO ()
main = play
  (InWindow "Haskell Tower Defense" (windowWidth, windowHeight) (100, 100))
  white
  60
  initialState
  render
  handleInput
  updateGame

 -- Initial game state
initialState :: GameState
initialState = GameState
  { towerHP = 100
  , doodleText = "Hello"
  , enemies = []
  , towers = []
  , towerSpots =
      [ (-275, 10), (-275, -70)
      , (-175, 10),  (-175, -70)
      , (-75, 10),   (-75, -70)
      , (25, 40),  (25, -105)
      , (125, 70),  (125, -120)
      , (50, -20)
      ]
  , selectedTower = Archer
  , currentWave = 0
  , waveQueue = generateWaves !! 0
  , currentGroup = Nothing
  , enemySpawnTimer = 0
  , groupSpawnTimer = 0
  , wavePauseTimer = 0
  , coins = 100
  , gameStatus = Playing
  , isPaused = False
  , showPauseMenu = False
  }

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char '1') Down _ _) gs = gs { selectedTower = Archer }
handleInput (EventKey (Char '2') Down _ _) gs = gs { selectedTower = Cannon }
handleInput (EventKey (Char '3') Down _ _) gs = gs { selectedTower = Sniper }
handleInput (EventKey (Char 'p') Down _ _) gs =
  gs { isPaused = not (isPaused gs), showPauseMenu = not (isPaused gs) }
handleInput (EventKey (MouseButton LeftButton) Up _ clickPos) gs =
  handleClick clickPos gs
handleInput _ gs = gs



handleClick :: (Float, Float) -> GameState -> GameState
handleClick (x, y) gs
  -- Pause button clicked
  | insideRect (x, y) pauseButtonPos pauseButtonSize =
      gs { isPaused = True, showPauseMenu = True }

  -- Resume/quit pause menu
  | showPauseMenu gs && insideRect (x, y) resumeButtonPos menuButtonSize =
      gs { isPaused = False, showPauseMenu = False }
  | showPauseMenu gs && insideRect (x, y) quitButtonPos menuButtonSize =
      gs { gameStatus = Defeat, isPaused = False, showPauseMenu = False }

  -- Shop button clicked
  | Just selected <- clickedTower (x, y) = gs { selectedTower = selected }

  -- Otherwise try placing tower
  | otherwise = tryPlaceTower (x, y) gs


clickedTower :: (Float, Float) -> Maybe TowerType
clickedTower (mx, my) =
  case filter (\(_, (cx, cy), (w, h)) ->
                 abs (mx - cx) <= w / 2 && abs (my - cy) <= h / 2
              ) shopButtonRects of
    ((ttype, _, _):_) -> Just ttype
    _ -> Nothing



insideRect :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
insideRect (mx, my) (cx, cy) (w, h) =
  abs (mx - cx) <= w / 2 && abs (my - cy) <= h / 2

tryPlaceTower :: (Float, Float) -> GameState -> GameState
tryPlaceTower click gs =
  case find (isClose click) (towerSpots gs) of
    Just spot ->
      let cost = towerCost (selectedTower gs)
      in if coins gs >= cost
         then
           let newTower = Tower (selectedTower gs) spot 0.0 Nothing
               remainingSpots = filter (/= spot) (towerSpots gs)
           in gs { towers = newTower : towers gs
                 , towerSpots = remainingSpots
                 , coins = coins gs - cost
                 }
         else gs
    Nothing -> gs
  where
    isClose (x1, y1) (x2, y2) = abs (x1 - x2) < 20 && abs (y1 - y2) < 20



updateGame :: Float -> GameState -> GameState
updateGame dt gs
  | gameStatus gs /= Playing = gs
  | isPaused gs              = gs
  | otherwise =
      let gs1 = updateWaveSystem dt gs
          (stillOnMap, reachedTower) = updateEnemies dt (enemies gs1)
          hpLoss = (length reachedTower * 40)
          newHP = towerHP gs1 - hpLoss

          (updatedTowers, damagedEnemies) = applyTowerDamage (towers gs1) stillOnMap
          (aliveEnemies, coinGain) = gainCoinsOnKills 5 damagedEnemies
          cooledTowers = updateTowersCooldown dt updatedTowers

          newStatus
            | newHP <= 0 = Defeat
            | null aliveEnemies
              && null (waveQueue gs1)
              && currentGroup gs1 == Nothing
              && currentWave gs1 == length generateWaves - 1
              = Victory
            | otherwise = Playing

      in gs1 { enemies    = aliveEnemies
             , towers     = cooledTowers
             , towerHP    = newHP
             , coins      = coins gs1 + coinGain
             , gameStatus = newStatus
             }



