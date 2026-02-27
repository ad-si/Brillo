{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brillo
import Brillo.Interface.Pure.Game
import Data.Text qualified as T
import System.Random (StdGen, getStdGen, randomR)


cols :: Int
cols = 32


rows :: Int
rows = 24


cellSize :: Float
cellSize = 20


data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq)


type Cell = (Int, Int)


data Phase = WaitingToStart | Playing | Dead
  deriving (Eq)


data GameState = GameState
  { snake :: [Cell]
  , food :: Cell
  , direction :: Direction
  , phase :: Phase
  , randomGen :: StdGen
  , score :: Int
  }


dirVector :: Direction -> Cell
dirVector DirUp = (0, -1)
dirVector DirDown = (0, 1)
dirVector DirLeft = (-1, 0)
dirVector DirRight = (1, 0)


opposite :: Direction -> Direction
opposite DirUp = DirDown
opposite DirDown = DirUp
opposite DirLeft = DirRight
opposite DirRight = DirLeft


moveSnake :: Cell -> Direction -> [Cell] -> (Bool, [Cell])
moveSnake _ _ [] = (False, [])
moveSnake foodCell dir snk@((hx, hy) : _) =
  let (dx, dy) = dirVector dir
      newHead = (hx + dx, hy + dy)
  in  if newHead == foodCell
        then (True, newHead : snk)
        else (False, newHead : init snk)


isDead :: [Cell] -> Bool
isDead [] = True
isDead ((hx, hy) : t) =
  hx <= 0 || hx >= cols || hy <= 0 || hy >= rows || (hx, hy) `elem` t


generateFood :: [Cell] -> StdGen -> (Cell, StdGen)
generateFood snk gen
  | newFood `elem` snk = generateFood snk gen3
  | otherwise = (newFood, gen3)
  where
    (fx, gen2) = randomR (1, cols - 1) gen
    (fy, gen3) = randomR (1, rows - 1) gen2
    newFood = (fx, fy)


-- Keep the head at least 5 cells from any wall so the initial
-- snake body fits and there's room to move before hitting a wall.
initialState :: Phase -> StdGen -> GameState
initialState p gen0 =
  let margin = 5
      (hx, gen1) = randomR (margin, cols - margin) gen0
      (hy, gen2) = randomR (margin, rows - margin) gen1
      (dirIdx, gen3) = randomR (0 :: Int, 3) gen2
      dir = [DirUp, DirDown, DirLeft, DirRight] !! dirIdx
      (dx, dy) = dirVector (opposite dir)
      body = [(hx + dx * i, hy + dy * i) | i <- [0 .. 4]]
      (foodCell, gen4) = generateFood body gen3
  in  GameState
        { snake = body
        , food = foodCell
        , direction = dir
        , phase = p
        , randomGen = gen4
        , score = 0
        }


-- Rendering

render :: GameState -> Picture
render gs =
  Pictures $
    concat
      [ walls
      , drawSnake gs.snake
      , [drawCell red gs.food]
      , [scoreText]
      , overlay
      ]
  where
    toScreen :: Cell -> (Float, Float)
    toScreen (x, y) =
      ( fromIntegral x * cellSize - 320
      , fromIntegral y * cellSize - 240
      )

    drawCell :: Color -> Cell -> Picture
    drawCell clr cell =
      let (sx, sy) = toScreen cell
      in  Color clr $
            Scale 1 (-1) $
              Translate sx sy $
                rectangleSolid cellSize cellSize

    drawSnake :: [Cell] -> [Picture]
    drawSnake [] = []
    drawSnake (h : t) = drawCell green h : map (drawCell (dark green)) t

    wallRect :: (Float, Float) -> (Float, Float) -> Picture
    wallRect (tx, ty) (w, h) =
      Color (greyN 0.4) $
        Scale 1 (-1) $
          Translate (tx * cellSize - 320) (ty * cellSize - 240) $
            rectangleSolid w h

    walls =
      [ wallRect (16, 0) (640, cellSize)
      , wallRect (16, 24) (640, cellSize)
      , wallRect (0, 12) (cellSize, 480)
      , wallRect (32, 12) (cellSize, 480)
      ]

    scoreText =
      Color white $
        Translate (-300) 220 $
          Scale 0.15 0.15 $
            Text $
              T.pack $
                "Score: " ++ show gs.score

    overlay = case phase gs of
      WaitingToStart ->
        [ Color white $
            Translate (-155) 0 $
              Scale 0.2 0.2 $
                Text "Press SPACE to start."
        ]
      Dead ->
        [ Color red $
            Translate (-200) 0 $
              Scale 0.5 0.5 $
                Text "GAME OVER"
        , Color white $
            Translate (-175) (-50) $
              Scale 0.2 0.2 $
                Text "Press SPACE to try again."
        ]
      Playing -> []


-- Input handling

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) gs = changeDirection gs DirLeft
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs = changeDirection gs DirRight
handleInput (EventKey (SpecialKey KeyUp) Down _ _) gs = changeDirection gs DirUp
handleInput (EventKey (SpecialKey KeyDown) Down _ _) gs = changeDirection gs DirDown
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = case phase gs of
  WaitingToStart -> gs{phase = Playing}
  Dead -> initialState Playing gs.randomGen
  Playing -> gs
handleInput _ gs = gs


changeDirection :: GameState -> Direction -> GameState
changeDirection gs newDir
  | newDir == opposite gs.direction = gs
  | otherwise = gs{direction = newDir}


-- Update

update :: Float -> GameState -> GameState
update _ gs
  | phase gs /= Playing = gs
  | otherwise =
      let (ate, newSnake) = moveSnake gs.food gs.direction gs.snake
          (newFood, newGen) = generateFood newSnake gs.randomGen
      in  gs
            { snake = newSnake
            , food = if ate then newFood else gs.food
            , phase = if isDead newSnake then Dead else Playing
            , randomGen = if ate then newGen else gs.randomGen
            , score = if ate then gs.score + 1 else gs.score
            }


main :: IO ()
main = do
  gen <- getStdGen
  play
    (InWindow "Snake" (640, 480) (100, 100))
    black
    10
    (initialState WaitingToStart gen)
    render
    handleInput
    update
