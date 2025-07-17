-- Main game module for Algorithms Defense (Gloss-based tower defense)

module Main where

-- Graphics and interface imports
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (find, partition)
import Graphics.Gloss.Juicy (loadJuicyJPG)
import Data.Maybe (catMaybes)
import System.Random
import Debug.Trace (trace)

-- Game-specific logic
import Game.Types
import Game.Shop
import Game.Logic
import Game.Render
import Game.Config
import Game.Enemies

-- | Main entry point
main :: IO ()
main = do
  gen <- newStdGen
  -- Load intro images from disk
  rawImages <- mapM (loadJuicyJPG . (\n -> "algorithms-defence/assets/" ++ show n ++ " frame.jpg")) [1..6]
  let images = catMaybes rawImages  -- Filter out failed image loads
      (waves, newGen) = generateRandomWaves gen 3  -- Generate 9 + 1 random enemy waves
      state = initialState waves newGen images     -- Build initial game state
  -- Launch the game window
  play
    (InWindow "Haskell Tower Defense" (windowWidth, windowHeight) (100, 100))
    white
    60
    state
    render
    handleInput
    update

-- | Create the initial game state with default values
initialState :: [[[Enemy]]] -> StdGen -> [Picture] -> GameState
initialState waves gen images = GameState
  { gameStatus = Intro 0 0
  , towerHP = 100
  , enemies = []
  , towers = []
  , towerSpots =  -- Predefined tower placement locations
      [ (-275, 10), (-275, -70)
      , (-175, 10),  (-175, -70)
      , (-75, 10),   (-75, -70)
      , (25, 40),  (25, -105)
      , (125, 70),  (125, -120)
      , (50, -20)
      ]
  , selectedTower = Archer
  , selectedTowerForMod = Nothing
  , currentWave = 0
  , waveQueue = waves
  , currentGroup = Nothing
  , enemySpawnTimer = 0
  , groupSpawnTimer = 0
  , wavePauseTimer = 0
  , randomGen = gen
  , coins = 100
  , isPaused = False
  , showPauseMenu = False
  , introImages = images
  , selectedModificator = Nothing
  }

-- | Handle user input events
handleInput :: Event -> GameState -> GameState

-- Tower selection via keys 1-3
handleInput (EventKey (Char '1') Down _ _) gs = gs { selectedTower = Archer }
handleInput (EventKey (Char '2') Down _ _) gs = gs { selectedTower = Cannon }
handleInput (EventKey (Char '3') Down _ _) gs = gs { selectedTower = Sniper }

-- Toggle pause with 'p'
handleInput (EventKey (Char 'p') Down _ _) gs =
  gs { isPaused = not (isPaused gs), showPauseMenu = not (isPaused gs) }

-- Handle mouse clicks
handleInput (EventKey (MouseButton LeftButton) Down _ clickPos) gs =
  case gameStatus gs of
    -- Advance through intro slides
    Intro idx _ -> 
      let images = introImages gs
          nextIdx = (idx + 1) `mod` length images
      in if nextIdx == 0
         then gs { gameStatus = Playing }
         else gs { gameStatus = Intro nextIdx 0 }

    -- In-game click (placing towers, UI interaction, etc.)
    Playing -> handleClick clickPos gs

    -- Ignore clicks in other states
    _ -> gs

-- Pressing space skips the intro
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs =
  case gameStatus gs of
    Intro _ _ -> gs { gameStatus = Playing }
    _ -> gs

-- Ignore unhandled events
handleInput _ gs = gs

-- | Handle mouse click interactions during gameplay
handleClick :: Position -> GameState -> GameState
handleClick (x, y) gs
  -- Clicked pause button
  | insideRect (x, y) pauseButtonPos pauseButtonSize =
      gs { isPaused = True, showPauseMenu = True }

  -- Pause menu: resume
  | showPauseMenu gs && insideRect (x, y) resumeButtonPos menuButtonSize =
      gs { isPaused = False, showPauseMenu = False }

  -- Pause menu: quit
  | showPauseMenu gs && insideRect (x, y) quitButtonPos menuButtonSize =
      gs { gameStatus = Defeat, isPaused = False, showPauseMenu = False }

  -- Clicked a modificator
  | Just mod <- getModificatorAt x y =
      if coins gs >= modificatorCost mod
         then gs { selectedModificator = Just mod }
         else gs  -- Not enough coins

  -- Applying a modificator to a tower
  | Just mod <- selectedModificator gs
  , Just tower <- findTowerAt (x, y) (towers gs) =
      if canApplyModificator mod tower
        then let updatedTowers = map
                      (\t -> if towerPos t == towerPos tower
                            then applyModificator mod t
                            else t)
                      (towers gs)
              in gs { towers = updatedTowers
                    , coins  = coins gs - modificatorCost mod
                    , selectedModificator = Nothing
                    }
        else gs { selectedModificator = Nothing }

  -- Clicked a tower selection button (in shop)
  | Just selected <- clickedTower (x, y) =
      gs { selectedTower = selected
         , selectedModificator = Nothing }

  -- Try to place a new tower at clicked location
  | otherwise = tryPlaceTower (x, y) gs { selectedModificator = Nothing }

-- | Detect if a tower button was clicked in the shop
clickedTower :: (Float, Float) -> Maybe TowerType
clickedTower (mx, my) =
  case filter (\(_, (cx, cy), (w, h)) ->
                 abs (mx - cx) <= w / 2 && abs (my - cy) <= h / 2
              ) shopButtonRects of
    ((ttype, _, _):_) -> Just ttype
    _ -> Nothing

-- | Check if a point is inside a given rectangle
insideRect :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
insideRect (mx, my) (cx, cy) (w, h) =
  abs (mx - cx) <= w / 2 && abs (my - cy) <= h / 2

-- | Try to place a tower at a valid nearby tower spot
tryPlaceTower :: (Float, Float) -> GameState -> GameState
tryPlaceTower click gs =
  case find (isClose click) (towerSpots gs) of
    Just spot
      | coins gs >= cost ->
          let newTower = Tower (selectedTower gs) spot 0.0 Nothing Nothing
              remainingSpots = filter (/= spot) (towerSpots gs)
          in gs { towers     = newTower : towers gs
                , towerSpots = remainingSpots
                , coins      = coins gs - cost
                }
      | otherwise -> gs
      where cost = towerCost (selectedTower gs)
    Nothing -> gs

-- | Check if two points are within 20 units (used to validate tower placement)
isClose (x1, y1) (x2, y2) = abs (x1 - x2) < 20 && abs (y1 - y2) < 20

-- | Main update function called every frame
update :: Float -> GameState -> GameState
update dt gs = case gameStatus gs of
  -- Intro image slideshow
  Intro idx t ->
    let t' = t + dt
        images = introImages gs
    in if t' > imageDuration
       then let nextIdx = (idx + 1) `mod` length images
            in if nextIdx == 0
               then gs { gameStatus = Playing }  -- Enter game after final image
               else gs { gameStatus = Intro nextIdx 0 }
       else gs { gameStatus = Intro idx t' }

  -- Delegate to actual game update
  _ -> updateGame dt gs

-- | Handle all logic updates during active gameplay
updateGame :: Float -> GameState -> GameState
updateGame dt gs
  | gameStatus gs /= Playing = gs  -- Do nothing unless actively playing
  | isPaused gs = gs               -- Do nothing if paused
  | otherwise =
      let gs1 = updateWaveSystem dt gs
          (stillOnMap, reachedTower) = updateEnemies dt (enemies gs1)
          hpLoss = updateEnemyDamage reachedTower
          newHP = max 0 (towerHP gs1 - hpLoss)
          (updatedTowers, damagedEnemies) = applyTowerDamage (towers gs1) stillOnMap
          (aliveEnemies, coinGain, bossChildren) = gainCoinsOnKillsWithBoss 5 damagedEnemies
          cooledTowers = updateTowersCooldown dt updatedTowers
          finalEnemies = aliveEnemies ++ bossChildren
          newStatus
            | newHP <= 0 = Defeat
            | null finalEnemies
              && null (waveQueue gs1)
              && currentGroup gs1 == Nothing
              && currentWave gs1 == 9 = Victory  -- 9 random waves completed
            | otherwise = Playing
      in gs1 { enemies = finalEnemies
             , towers = cooledTowers
             , towerHP = newHP
             , coins = coins gs1 + coinGain
             , gameStatus = newStatus
             }
