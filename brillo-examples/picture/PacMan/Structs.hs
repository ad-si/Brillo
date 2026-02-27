module Structs where

import Brillo (Point)


type Velocity = (Float, Float)


fps :: Int
fps = 60


frightenLength :: Float
frightenLength = 4.0


data Name = Pacman | Pinky | Inky | Blinky | Clyde
  deriving (Eq)


data Direction = UP | DOWN | LEFT | RIGHT | STOP
  deriving (Eq, Show, Ord, Enum, Bounded)


data GameStatus = WON | LOST | PLAYING
  deriving (Eq, Show)


data Mode = SCATTER | CHASE | FRIGHTENED
  deriving (Eq, Show)


blinkyScatterTarget :: Point
blinkyScatterTarget = (8, 10)


inkyScatterTarget :: Point
inkyScatterTarget = (8, -12)


pinkyScatterTarget :: Point
pinkyScatterTarget = (-8, 10)


clydeScatterTarget :: Point
clydeScatterTarget = (-8, -12)


data Characters = Characters
  { cName :: Name
  , speed :: Velocity
  , location :: Point
  }
  deriving (Eq)


data Ghost = Ghost
  { gName :: Name
  , gSpeed :: Velocity
  , gLocation :: Point
  , gTarget :: Point
  , gLastMove :: Direction
  , gDirection :: Direction
  }


data PacGame = Game
  { lives :: Int
  , gameStatus :: GameStatus
  , pacman :: Characters
  , pinky :: Ghost
  , inky :: Ghost
  , blinky :: Ghost
  , clyde :: Ghost
  , time :: Float
  , score :: Int
  , pellets :: [Point]
  , direction :: Direction
  , bufDirection :: Direction
  , gMode :: Mode
  , lcrB :: Point
  , lcrP :: Point
  , lcrI :: Point
  , lcrC :: Point
  , pPellets :: [Point]
  , fTime :: Float
  }
