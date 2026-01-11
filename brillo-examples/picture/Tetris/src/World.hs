{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module World where

import Control.Monad.Reader
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord
import System.Random

import Config
import Figures
import Util


type Grid = Map.Map (GridPosition, GameColor) ()


data Hardness = Noob | Beginner | Average | Skilled | Masterful | Insane | Godlike
  deriving (Enum, Bounded, Show)


noobBound = 0
beginnerBound = 300
averageBound = 800
skilledBound = 1400
masterfulBound = 2200
insaneBound = 3500
godlikeBound = 5000


-- COLORS

data GameColor = Red | Yellow | Green | Blue | Violet
  deriving (Enum, Bounded, Ord, Eq)


-- Instance of class Random for Colors
instance Random GameColor where
  random g = randomR (minBound, maxBound) g
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')


-- | Data represents the state of the Tetris game
data TetrisGame = Game
  { fallingFigure :: Figure
  , fallingPosition :: GridPosition
  , startFalling :: GridPosition
  , fallingColor :: GameColor
  , nextColors :: [GameColor]
  , width :: Int
  , height :: Int
  , nextFigures :: [Figure]
  , grid :: Grid
  , hardness :: Hardness
  , score :: Integer
  , frapsCounter :: Integer
  , isPause :: Bool
  , gameOver :: Bool
  , pressedDown :: Bool
  }


-- | Initial state of the game
initialState :: ReaderT AppConfig IO TetrisGame
initialState = do
  cfg <- ask
  gen <- liftIO getStdGen
  let (f : fs) = randomFigures gen
  let (c : cs) = randoms gen
  let startPos = startPosition cfg
  return $
    Game
      f
      startPos
      startPos
      c
      cs
      (fst $ gridSize $ cfg)
      (snd $ gridSize $ cfg)
      fs
      Map.empty
      minBound
      0
      0
      False
      False
      False


-- | Real position in Grid
getRealCoords :: Figure -> GridPosition -> [Block]
getRealCoords (Figure _ _ bs) curPos = map (sumPair curPos) bs


-- | List of random figures
randomFigures :: (RandomGen g) => g -> [Figure]
randomFigures gen = zipWith getFigures (randoms gen) (randoms gen)


-- | Sets the currently falling figure from nextFigures
nextFigureGame :: TetrisGame -> TetrisGame
nextFigureGame g@Game{..}
  | checkingGO = g{gameOver = True}
  | (nf : nfs) <- nextFigures
  , (nc : ncs) <- nextColors =
      updateHardness $
        updateScore $
          Game
            nf
            startFalling
            startFalling
            nc
            ncs
            width
            height
            nfs
            updateGrid
            hardness
            score
            frapsCounter
            isPause
            checkingGO
            pressedDown
  | otherwise = g{gameOver = True}
  where
    updateScore gnew = gnew{score = score + (getScore $ countOfBurns g gnew)}

    countOfBurns (length . getGridAsList -> countOld) (length . getGridAsList -> countNew) = div ((countOld + 4) - countNew) width

    -- Returns score based on count of burned lines.
    getScore :: Int -> Integer
    getScore 0 = 0
    getScore 1 = 100
    getScore 2 = 300
    getScore 3 = 700
    getScore 4 = 1500

    updateHardness :: TetrisGame -> TetrisGame
    updateHardness g@(Game _ _ _ _ _ _ _ _ _ _ scr _ _ _ _) = g{hardness = nextHardness scr}

    nextHardness :: Integer -> Hardness
    nextHardness scr
      | scr < beginnerBound = Noob
      | scr < averageBound = Beginner
      | scr < skilledBound = Average
      | scr < masterfulBound = Skilled
      | scr < insaneBound = Masterful
      | scr < godlikeBound = Insane
      | otherwise = Godlike

    updateGrid =
      burnFullLines $
        foldl
          (\gr bl -> (Map.insert (bl, fallingColor) () gr))
          grid
          (getRealCoords fallingFigure fallingPosition)

    checkingGO = any (\(_, y) -> y >= height) (getRealCoords fallingFigure fallingPosition)

    burnFullLines =
      listToGrid
        . concat
        . zipWith
          (\num list -> map (\((x, y), color) -> ((x, num), color)) list)
          [0, 1 ..]
        . filter ((/= width) . length)
        . groupBy (\((_, y1), _) ((_, y2), _) -> y1 == y2)
        . sortBy (comparing (snd . fst))
        . gridToList


-- | Shifts left a figure if able to
shiftLeftFigure :: TetrisGame -> TetrisGame
shiftLeftFigure curTetrisGame@(Game ff (shiftLeft -> fpos) spos fc nc w h fs grid hrd scr fcnt isPs go km)
  | goodCoords grid w h (getRealCoords ff fpos) =
      Game ff fpos spos fc nc w h fs grid hrd scr fcnt isPs go km
  | otherwise = curTetrisGame


-- | Shifts right a figure if able to
shiftRightFigure :: TetrisGame -> TetrisGame
shiftRightFigure curTetrisGame@(Game ff (shiftRight -> fpos) spos fc nc w h fs grid hrd scr fcnt isPs go km)
  | goodCoords grid w h (getRealCoords ff fpos) =
      Game ff fpos spos fc nc w h fs grid hrd scr fcnt isPs go km
  | otherwise = curTetrisGame


-- | Shifts down a figure if able to
shiftDownFigure :: TetrisGame -> TetrisGame
shiftDownFigure curTetrisGame@(Game ff (shiftDown -> fpos) spos fc nc w h fs grid hrd scr fcnt isPs go km)
  | goodCoords grid w h (getRealCoords ff fpos) =
      Game ff fpos spos fc nc w h fs grid hrd scr fcnt isPs go km
  | otherwise = nextFigureGame curTetrisGame


-- | Rotates a figure if able to
rotateFigure :: TetrisGame -> TetrisGame
rotateFigure curTetrisGame@(Game (rotate -> ff) fpos spos fc nc w h fs grid hrd scr fcnt isPs go km)
  | goodCoords grid w h (getRealCoords ff fpos) =
      Game ff fpos spos fc nc w h fs grid hrd scr fcnt isPs go km
  | otherwise = curTetrisGame


-- | Instantly drops a figure to the bottom
dropFigure :: TetrisGame -> TetrisGame
dropFigure game = dropUntilCollision game
  where
    dropUntilCollision currentGame@(Game ff (shiftDown -> fpos) spos fc nc w h fs grid hrd scr fcnt isPs go km) =
      if goodCoords grid w h (getRealCoords ff fpos)
        then
          dropUntilCollision (Game ff fpos spos fc nc w h fs grid hrd scr fcnt isPs go km)
        else nextFigureGame currentGame -- Can't move down anymore, lock the piece


resetGame :: TetrisGame -> TetrisGame
resetGame Game{..}
  | (_ : nf : nfs) <- nextFigures
  , (_ : nc : ncs) <- nextColors =
      Game
        nf
        startFalling
        startFalling
        nc
        ncs
        width
        height
        nfs
        Map.empty
        minBound
        0
        0
        False
        False
        pressedDown
  | otherwise = error "resetGame: insufficient figures or colors"


pressedKeyDown :: TetrisGame -> Bool
pressedKeyDown Game{..} = pressedDown


-- | Checks that the point belongs to the Grid and that it is free
goodCoords :: Grid -> Int -> Int -> [Block] -> Bool
goodCoords grid w h = all goodCoord
  where
    goodCoord pos@(x, y) =
      x >= 0
        && x < w
        && y >= 0
        && ((== 0) . (length) . (filter (\(grPos, _) -> grPos == pos)) . gridToList) grid


-- | Returns next figure
getNextFigure :: TetrisGame -> Figure
getNextFigure (nextFigures -> (f : _)) = f
getNextFigure _ = error "getNextFigure: no next figure available"


getGridAsList :: TetrisGame -> [(GridPosition, GameColor)]
getGridAsList (grid -> gr) = gridToList gr


listToGrid :: [(GridPosition, GameColor)] -> Grid
listToGrid = Map.fromList . (`zip` repeat ())


gridToList :: Grid -> [(GridPosition, GameColor)]
gridToList = Map.keys


shiftRight :: GridPosition -> GridPosition
shiftRight = sumPair (1, 0)


shiftLeft :: GridPosition -> GridPosition
shiftLeft = sumPair (-1, 0)


shiftDown :: GridPosition -> GridPosition
shiftDown = sumPair (0, -1)
