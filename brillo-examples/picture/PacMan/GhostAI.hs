{-# LANGUAGE OverloadedRecordDot #-}

module GhostAI where

import Brillo
import Maze
import Structs


-- Round a Float to the nearest integer, returning Float
roundF :: Float -> Float
roundF x = fromIntegral (round x :: Int)


listLength :: [a] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs


checkInList :: (Eq a) => a -> [a] -> Bool
checkInList x l
  | x `elem` l = False
  | otherwise = True


-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
-- add 3 seconds to everything because we stop the first 3 seconds
-- of the game for the start screen
ghostModeTimer :: PacGame -> PacGame
ghostModeTimer game
  | curTime < 10 =
      game
        { gMode = SCATTER
        , blinky = setGhost game.blinky blinkyScatterTarget
        , pinky = setGhost game.pinky pinkyScatterTarget
        , inky = setGhost game.inky inkyScatterTarget
        , clyde = setGhost game.clyde clydeScatterTarget
        }
  | curTime >= 10 && curTime < 30 =
      game
        { gMode = CHASE
        , blinky = setGhost game.blinky (x', y')
        , pinky = setGhost game.pinky (x' + 3, y' + 3)
        , inky = setGhost game.inky targetI
        , clyde = setGhost game.clyde targetC
        }
  | curTime >= 30 && curTime < 37 =
      game
        { gMode = SCATTER
        , blinky = setGhost game.blinky blinkyScatterTarget
        , pinky = setGhost game.pinky pinkyScatterTarget
        , inky = setGhost game.inky inkyScatterTarget
        , clyde = setGhost game.clyde clydeScatterTarget
        }
  | curTime >= 37 && curTime < 57 =
      game
        { gMode = CHASE
        , blinky = setGhost game.blinky (x', y')
        , pinky = setGhost game.pinky (x' + 3, y' + 3)
        , inky = setGhost game.inky targetI
        , clyde = setGhost game.clyde targetC
        }
  | curTime >= 57 && curTime < 62 =
      game
        { gMode = SCATTER
        , blinky = setGhost game.blinky blinkyScatterTarget
        , pinky = setGhost game.pinky pinkyScatterTarget
        , inky = setGhost game.inky inkyScatterTarget
        , clyde = setGhost game.clyde clydeScatterTarget
        }
  | curTime >= 62 && curTime < 82 =
      game
        { gMode = CHASE
        , blinky = setGhost game.blinky (x', y')
        , pinky = setGhost game.pinky (x' + 3, y' + 3)
        , inky = setGhost game.inky targetI
        , clyde = setGhost game.clyde targetC
        }
  | curTime >= 82 && curTime < 87 =
      game
        { gMode = SCATTER
        , blinky = setGhost game.blinky blinkyScatterTarget
        , pinky = setGhost game.pinky pinkyScatterTarget
        , inky = setGhost game.inky inkyScatterTarget
        , clyde = setGhost game.clyde clydeScatterTarget
        }
  | curTime >= 87 && curTime < 107 =
      game
        { gMode = CHASE
        , blinky = setGhost game.blinky (x', y')
        , pinky = setGhost game.pinky (x' + 3, y' + 3)
        , inky = setGhost game.inky targetI
        , clyde = setGhost game.clyde targetC
        }
  | curTime >= 107 && curTime < 112 =
      game
        { gMode = SCATTER
        , blinky = setGhost game.blinky blinkyScatterTarget
        , pinky = setGhost game.pinky pinkyScatterTarget
        , inky = setGhost game.inky inkyScatterTarget
        , clyde = setGhost game.clyde clydeScatterTarget
        }
  | otherwise =
      game
        { gMode = CHASE
        , blinky = setGhost game.blinky (x', y')
        , pinky = setGhost game.pinky (x' + 3, y' + 3)
        , inky = setGhost game.inky targetI
        , clyde = setGhost game.clyde targetC
        }
  where
    curTime = game.time
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    targetI = targetInky game
    targetC = targetClyde game
    setGhost g t = g{gTarget = t}


ghostModeSwitch :: PacGame -> PacGame
ghostModeSwitch game
  | curMode == SCATTER || curMode == CHASE = ghostModeTimer game
  | frTime >= frightenLength = ghostModeTimer game
  | frTime < frightenLength = game{fTime = frTime + 1 / fromIntegral fps}
  | otherwise = game
  where
    curMode = game.gMode
    frTime = game.fTime


releaseInky :: PacGame -> PacGame
releaseInky game =
  if curTime > 4
    && dotsLeft >= 10
    && (px_py == (-1, 0) || px_py == (0, 0) || px_py == (1, 0))
    then
      game
        { inky =
            game.inky
              { gSpeed = (0, 0)
              , gLocation = (0, 2)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    else game
  where
    curTime = game.time
    dotsLeft = listLength pelletsL - listLength (game.pellets)
    px_py = game.inky.gLocation


releaseClyde :: PacGame -> PacGame
releaseClyde game =
  if curTime > 4
    && dotsLeft >= 50
    && ((cx', cy') == (-1, 0) || (cx', cy') == (0, 0) || (cx', cy') == (1, 0))
    then
      game
        { clyde =
            game.clyde
              { gSpeed = (0, 0)
              , gLocation = (0, 2)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    else game
  where
    curTime = game.time
    dotsLeft = listLength pelletsL - listLength (game.pellets)
    (cx, cy) = game.clyde.gLocation
    cx' = roundF cx
    cy' = roundF cy


releasePinky :: PacGame -> PacGame
releasePinky game =
  if curTime > 4 && posPinky == (0, 0)
    then
      game
        { pinky =
            game.pinky
              { gSpeed = (0, 0)
              , gLocation = (0, 2)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    else game
  where
    curTime = game.time
    posPinky = game.pinky.gLocation


distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1


ghostCollision :: PacGame -> PacGame
ghostCollision game
  | (x', y') == (bx', by') = checkLives
  | (x', y') == (px', py') = checkLives
  | (x', y') == (ix', iy') = checkLives
  | (x', y') == (cx', cy') = checkLives
  | otherwise = game
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    (bx, by) = game.blinky.gLocation
    (px, py) = game.pinky.gLocation
    (ix, iy) = game.inky.gLocation
    (cx, cy) = game.clyde.gLocation
    bx' = roundF bx
    by' = roundF by
    px' = roundF px
    py' = roundF py
    ix' = roundF ix
    iy' = roundF iy
    cx' = roundF cx
    cy' = roundF cy
    curLives = game.lives
    checkLives =
      if curLives > 1
        then
          game
            { gMode = SCATTER
            , lcrB = (0, 0)
            , lcrP = (0, 0)
            , lcrI = (0, 0)
            , lcrC = (0, 0)
            , time = 0.0
            , direction = STOP
            , bufDirection = STOP
            , lives = curLives - 1
            , pacman = Characters{cName = Pacman, speed = (0, 0), location = (0, -6)}
            , pinky =
                Ghost
                  { gName = Pinky
                  , gSpeed = (0, 0)
                  , gLocation = (0, 0)
                  , gTarget = pinkyScatterTarget
                  , gLastMove = STOP
                  , gDirection = STOP
                  }
            , inky =
                Ghost
                  { gName = Inky
                  , gSpeed = (0, 0)
                  , gLocation = (-1, 0)
                  , gTarget = inkyScatterTarget
                  , gLastMove = STOP
                  , gDirection = STOP
                  }
            , blinky =
                Ghost
                  { gName = Blinky
                  , gSpeed = (0, 0)
                  , gLocation = (0, 2)
                  , gTarget = blinkyScatterTarget
                  , gLastMove = STOP
                  , gDirection = STOP
                  }
            , clyde =
                Ghost
                  { gName = Clyde
                  , gSpeed = (0, 0)
                  , gLocation = (1, 0)
                  , gTarget = clydeScatterTarget
                  , gLastMove = STOP
                  , gDirection = STOP
                  }
            }
        else game{gameStatus = LOST}


ghostCollisionSwitch :: PacGame -> PacGame
ghostCollisionSwitch game
  | mode == FRIGHTENED = case checkEaten game of
      1 -> eatBlinky
      2 -> eatPinky
      3 -> eatInky
      4 -> eatClyde
      _ -> game
  | otherwise = ghostCollision game
  where
    eatBlinky =
      game
        { blinky =
            game.blinky
              { gSpeed = (0, 0)
              , gTarget = (0, 0)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    eatPinky =
      game
        { pinky =
            game.pinky
              { gSpeed = (0, 0)
              , gTarget = (0, 0)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    eatInky =
      game
        { inky =
            game.inky
              { gSpeed = (0, 0)
              , gTarget = (0, 0)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    eatClyde =
      game
        { clyde =
            game.clyde
              { gSpeed = (0, 0)
              , gTarget = (0, 0)
              , gLastMove = STOP
              , gDirection = STOP
              }
        }
    mode = game.gMode


checkEaten :: PacGame -> Int
checkEaten game
  | (x', y') == (bx', by') = 1
  | (x', y') == (px', py') = 2
  | (x', y') == (ix', iy') = 3
  | (x', y') == (cx', cy') = 4
  | otherwise = 5
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    (bx, by) = game.blinky.gLocation
    (px, py) = game.pinky.gLocation
    (ix, iy) = game.inky.gLocation
    (cx, cy) = game.clyde.gLocation
    bx' = roundF bx
    by' = roundF by
    px' = roundF px
    py' = roundF py
    ix' = roundF ix
    iy' = roundF iy
    cx' = roundF cx
    cy' = roundF cy


-------------------- BLINKY ------------------

moveToTargetB :: PacGame -> PacGame
moveToTargetB game
  | curTime <= 3 = game
  | elem (x', y') crossR && lastCR /= (x', y') =
      game
        { lcrB = (x', y')
        , blinky = game.blinky{gSpeed = (0, 0), gLocation = (x, y)}
        }
  | (vx, vy) == (0, 0) = case bWalls of
      1 -> b1
      2 -> b2
      3 -> b3
      4 -> b4
      5 -> b5
      _ -> b10
  | (vx, vy) /= (0, 0) = case game.blinky.gDirection of
      UP -> testUp
      DOWN -> testDown
      LEFT -> testLeft
      RIGHT -> testRight
      STOP -> stop
  | otherwise = game
  where
    (tx, ty) = game.blinky.gTarget
    (vx, vy) = game.blinky.gSpeed
    (x, y) = game.blinky.gLocation
    x' = roundF x
    y' = roundF y
    yU = y' + 1
    yD = y' - 1
    xL = x' - 1
    xR = x' + 1
    lastCR = game.lcrB
    curTarget = game.blinky.gTarget
    lm = game.blinky.gLastMove
    cD = game.blinky.gDirection
    curTime = game.time

    b1 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= LEFT
                    && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                    then goLeft
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= UP && checkInList (x', yU) coords
                            then goUp
                            else
                              if lm /= LEFT && checkInList (xL, y') coords
                                then goLeft
                                else
                                  if lm /= DOWN && checkInList (x', yD) coords
                                    then goDown
                                    else game

    b2 =
      if lm /= UP
        && distance (x', yU) (tx, ty) <= distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) <= distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) <= distance (x', yU) (tx, ty)
            && distance (xR, y') (tx, ty) <= distance (xL, y') (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) <= distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) <= distance (xR, y') (tx, ty)
                then goLeft
                else
                  if lm /= UP && checkInList (x', yU) coords
                    then goUp
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    b3 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    b4 =
      if lm /= DOWN
        && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
        then goDown
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
            && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    b5 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    b10 =
      if lm /= RIGHT && checkInList (xR, y') coords
        then goRight
        else
          if lm /= UP && checkInList (x', yU) coords
            then goUp
            else
              if lm /= LEFT && checkInList (xL, y') coords
                then goLeft
                else
                  if lm /= DOWN && checkInList (x', yD) coords
                    then goDown
                    else game

    bWalls = findWalls game 1

    goRight =
      game
        { blinky =
            Ghost
              { gName = Blinky
              , gSpeed = (3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = LEFT
              , gDirection = RIGHT
              }
        }
    goLeft =
      game
        { blinky =
            Ghost
              { gName = Blinky
              , gSpeed = (-3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = RIGHT
              , gDirection = LEFT
              }
        }
    goUp =
      game
        { blinky =
            Ghost
              { gName = Blinky
              , gSpeed = (0, 3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = DOWN
              , gDirection = UP
              }
        }
    goDown =
      game
        { blinky =
            Ghost
              { gName = Blinky
              , gSpeed = (0, -3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = UP
              , gDirection = DOWN
              }
        }

    testUp =
      if elem (x', yU) coords
        then
          game
            { blinky =
                Ghost
                  { gName = Blinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testDown =
      if elem (x', yD) coords
        then
          game
            { blinky =
                Ghost
                  { gName = Blinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testLeft =
      if elem (xL, y') coords
        then
          game
            { blinky =
                Ghost
                  { gName = Blinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testRight =
      if elem (xR, y') coords
        then
          game
            { blinky =
                Ghost
                  { gName = Blinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    stop =
      game
        { blinky =
            Ghost
              { gName = Blinky
              , gSpeed = (0, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = lm
              , gDirection = cD
              }
        }


-------------------- PINKY ------------------

moveToTargetP :: PacGame -> PacGame
moveToTargetP game
  | curTime <= 4 = game
  | elem (x', y') crossR && lastCR /= (x', y') =
      game{lcrP = (x', y'), pinky = game.pinky{gSpeed = (0, 0), gLocation = (x, y)}}
  | (vx, vy) == (0, 0) = case pWalls of
      1 -> p1
      2 -> p2
      3 -> p3
      4 -> p4
      5 -> p5
      _ -> p10
  | (vx, vy) /= (0, 0) = case game.pinky.gDirection of
      UP -> testUp
      DOWN -> testDown
      LEFT -> testLeft
      RIGHT -> testRight
      STOP -> stop
  | otherwise = game
  where
    (tx, ty) = game.pinky.gTarget
    (vx, vy) = game.pinky.gSpeed
    (x, y) = game.pinky.gLocation
    x' = roundF x
    y' = roundF y
    yU = y' + 1
    yD = y' - 1
    xL = x' - 1
    xR = x' + 1
    lastCR = game.lcrP
    curTarget = game.pinky.gTarget
    lm = game.pinky.gLastMove
    cD = game.pinky.gDirection
    curTime = game.time

    pWalls = findWalls game 2

    p1 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= LEFT
                    && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                    then goLeft
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= UP && checkInList (x', yU) coords
                            then goUp
                            else
                              if lm /= LEFT && checkInList (xL, y') coords
                                then goLeft
                                else
                                  if lm /= DOWN && checkInList (x', yD) coords
                                    then goDown
                                    else game

    p2 =
      if lm /= UP
        && distance (x', yU) (tx, ty) <= distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) <= distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) <= distance (x', yU) (tx, ty)
            && distance (xR, y') (tx, ty) <= distance (xL, y') (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) <= distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) <= distance (xR, y') (tx, ty)
                then goLeft
                else
                  if lm /= UP && checkInList (x', yU) coords
                    then goUp
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p3 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p4 =
      if lm /= DOWN
        && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
        then goDown
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
            && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p5 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p10 =
      if lm /= RIGHT && checkInList (xR, y') coords
        then goRight
        else
          if lm /= UP && checkInList (x', yU) coords
            then goUp
            else
              if lm /= LEFT && checkInList (xL, y') coords
                then goLeft
                else
                  if lm /= DOWN && checkInList (x', yD) coords
                    then goDown
                    else game

    goRight =
      game
        { pinky =
            Ghost
              { gName = Pinky
              , gSpeed = (3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = LEFT
              , gDirection = RIGHT
              }
        }
    goLeft =
      game
        { pinky =
            Ghost
              { gName = Pinky
              , gSpeed = (-3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = RIGHT
              , gDirection = LEFT
              }
        }
    goUp =
      game
        { pinky =
            Ghost
              { gName = Pinky
              , gSpeed = (0, 3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = DOWN
              , gDirection = UP
              }
        }
    goDown =
      game
        { pinky =
            Ghost
              { gName = Pinky
              , gSpeed = (0, -3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = UP
              , gDirection = DOWN
              }
        }

    testUp =
      if elem (x', yU) coords
        then
          game
            { pinky =
                Ghost
                  { gName = Pinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testDown =
      if elem (x', yD) coords
        then
          game
            { pinky =
                Ghost
                  { gName = Pinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testLeft =
      if elem (xL, y') coords
        then
          game
            { pinky =
                Ghost
                  { gName = Pinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testRight =
      if elem (xR, y') coords
        then
          game
            { pinky =
                Ghost
                  { gName = Pinky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    stop =
      game
        { pinky =
            Ghost
              { gName = Pinky
              , gSpeed = (0, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = lm
              , gDirection = cD
              }
        }


-------------------- INKY ------------------

targetInky :: PacGame -> Point
targetInky game = case pacDirection of
  UP -> up
  DOWN -> down
  LEFT -> left
  RIGHT -> right
  STOP -> up
  where
    (bx, by) = game.blinky.gLocation
    bx' = roundF bx
    by' = roundF by
    pacDirection = game.direction
    (px, py) = game.pacman.location
    px' = roundF px
    py' = roundF py
    up = ((px' - bx') + px', ((py' + 1) - by') + py')
    down = ((px' - bx') + px', ((py' - 1) - by') + py')
    left = (((px' - 1) - bx') + px', (py' - by') + py')
    right = (((px' + 1) - bx') + px', (py' - by') + py')


moveToTargetI :: PacGame -> PacGame
moveToTargetI game
  | curTime <= 4 = game
  | dotsLeft <= 10 = game
  | elem (x', y') crossR && lastCR /= (x', y') =
      game{lcrI = (x', y'), inky = game.inky{gSpeed = (0, 0), gLocation = (x, y)}}
  | (vx, vy) == (0, 0) = case iWalls of
      1 -> p1
      2 -> p2
      3 -> p3
      4 -> p4
      5 -> p5
      _ -> p10
  | (vx, vy) /= (0, 0) = case game.inky.gDirection of
      UP -> testUp
      DOWN -> testDown
      LEFT -> testLeft
      RIGHT -> testRight
      STOP -> stop
  | otherwise = game
  where
    curPellets = game.pellets
    dotsLeft = listLength pelletsL - listLength curPellets
    (tx, ty) = game.inky.gTarget
    (vx, vy) = game.inky.gSpeed
    (x, y) = game.inky.gLocation
    x' = roundF x
    y' = roundF y
    yU = y' + 1
    yD = y' - 1
    xL = x' - 1
    xR = x' + 1
    lastCR = game.lcrI
    curTarget = game.inky.gTarget
    lm = game.inky.gLastMove
    cD = game.inky.gDirection
    curTime = game.time

    iWalls = findWalls game 3

    p1 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= LEFT
                    && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                    then goLeft
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= UP && checkInList (x', yU) coords
                            then goUp
                            else
                              if lm /= LEFT && checkInList (xL, y') coords
                                then goLeft
                                else
                                  if lm /= DOWN && checkInList (x', yD) coords
                                    then goDown
                                    else game

    p2 =
      if lm /= UP
        && distance (x', yU) (tx, ty) <= distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) <= distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) <= distance (x', yU) (tx, ty)
            && distance (xR, y') (tx, ty) <= distance (xL, y') (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) <= distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) <= distance (xR, y') (tx, ty)
                then goLeft
                else
                  if lm /= UP && checkInList (x', yU) coords
                    then goUp
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p3 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p4 =
      if lm /= DOWN
        && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
        then goDown
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
            && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p5 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    p10 =
      if lm /= RIGHT && checkInList (xR, y') coords
        then goRight
        else
          if lm /= UP && checkInList (x', yU) coords
            then goUp
            else
              if lm /= LEFT && checkInList (xL, y') coords
                then goLeft
                else
                  if lm /= DOWN && checkInList (x', yD) coords
                    then goDown
                    else game

    goRight =
      game
        { inky =
            Ghost
              { gName = Inky
              , gSpeed = (3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = LEFT
              , gDirection = RIGHT
              }
        }
    goLeft =
      game
        { inky =
            Ghost
              { gName = Inky
              , gSpeed = (-3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = RIGHT
              , gDirection = LEFT
              }
        }
    goUp =
      game
        { inky =
            Ghost
              { gName = Inky
              , gSpeed = (0, 3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = DOWN
              , gDirection = UP
              }
        }
    goDown =
      game
        { inky =
            Ghost
              { gName = Inky
              , gSpeed = (0, -3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = UP
              , gDirection = DOWN
              }
        }

    testUp =
      if elem (x', yU) coords
        then
          game
            { inky =
                Ghost
                  { gName = Inky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testDown =
      if elem (x', yD) coords
        then
          game
            { inky =
                Ghost
                  { gName = Inky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testLeft =
      if elem (xL, y') coords
        then
          game
            { inky =
                Ghost
                  { gName = Inky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testRight =
      if elem (xR, y') coords
        then
          game
            { inky =
                Ghost
                  { gName = Inky
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    stop =
      game
        { inky =
            Ghost
              { gName = Inky
              , gSpeed = (0, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = lm
              , gDirection = cD
              }
        }


-------------------- CLYDE ------------------

targetClyde :: PacGame -> Point
targetClyde game
  | (px' - cx' <= 4) || (py' - cy' <= 4) = clydeScatterTarget
  | otherwise = (px', py')
  where
    (px, py) = game.pacman.location
    px' = roundF px
    py' = roundF py
    (cx, cy) = game.clyde.gLocation
    cx' = roundF cx
    cy' = roundF cy


moveToTargetC :: PacGame -> PacGame
moveToTargetC game
  | curTime <= 3 = game
  | dotsLeft <= 50 = game
  | elem (x', y') crossR && lastCR /= (x', y') =
      game{lcrC = (x', y'), clyde = game.clyde{gSpeed = (0, 0), gLocation = (x, y)}}
  | (vx, vy) == (0, 0) = case cWalls of
      1 -> c1
      2 -> c2
      3 -> c3
      4 -> c4
      5 -> c5
      _ -> c10
  | (vx, vy) /= (0, 0) = case game.clyde.gDirection of
      UP -> testUp
      DOWN -> testDown
      LEFT -> testLeft
      RIGHT -> testRight
      STOP -> stop
  | otherwise = game
  where
    curPellets = game.pellets
    dotsLeft = listLength pelletsL - listLength curPellets
    (tx, ty) = game.clyde.gTarget
    (vx, vy) = game.clyde.gSpeed
    (x, y) = game.clyde.gLocation
    x' = roundF x
    y' = roundF y
    yU = y' + 1
    yD = y' - 1
    xL = x' - 1
    xR = x' + 1
    lastCR = game.lcrC
    curTarget = game.clyde.gTarget
    lm = game.clyde.gLastMove
    cD = game.clyde.gDirection
    curTime = game.time

    c1 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= LEFT
                    && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                    && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                    then goLeft
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= UP && checkInList (x', yU) coords
                            then goUp
                            else
                              if lm /= LEFT && checkInList (xL, y') coords
                                then goLeft
                                else
                                  if lm /= DOWN && checkInList (x', yD) coords
                                    then goDown
                                    else game

    c2 =
      if lm /= UP
        && distance (x', yU) (tx, ty) <= distance (xR, y') (tx, ty)
        && distance (x', yU) (tx, ty) <= distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) <= distance (x', yU) (tx, ty)
            && distance (xR, y') (tx, ty) <= distance (xL, y') (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) <= distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) <= distance (xR, y') (tx, ty)
                then goLeft
                else
                  if lm /= UP && checkInList (x', yU) coords
                    then goUp
                    else
                      if lm /= RIGHT && checkInList (xR, y') coords
                        then goRight
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    c3 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xR, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
            then goDown
            else
              if lm /= RIGHT
                && distance (xR, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goRight
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    c4 =
      if lm /= DOWN
        && distance (x', yD) (tx, ty) < distance (xR, y') (tx, ty)
        && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
        then goDown
        else
          if lm /= RIGHT
            && distance (xR, y') (tx, ty) < distance (xL, y') (tx, ty)
            && distance (xR, y') (tx, ty) < distance (x', yD) (tx, ty)
            then goRight
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (xR, y') (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    c5 =
      if lm /= UP
        && distance (x', yU) (tx, ty) < distance (x', yD) (tx, ty)
        && distance (x', yU) (tx, ty) < distance (xL, y') (tx, ty)
        then goUp
        else
          if lm /= DOWN
            && distance (x', yD) (tx, ty) < distance (x', yU) (tx, ty)
            && distance (x', yD) (tx, ty) < distance (xL, y') (tx, ty)
            then goDown
            else
              if lm /= LEFT
                && distance (xL, y') (tx, ty) < distance (x', yU) (tx, ty)
                && distance (xL, y') (tx, ty) < distance (x', yD) (tx, ty)
                then goLeft
                else
                  if lm /= RIGHT && checkInList (xR, y') coords
                    then goRight
                    else
                      if lm /= UP && checkInList (x', yU) coords
                        then goUp
                        else
                          if lm /= LEFT && checkInList (xL, y') coords
                            then goLeft
                            else
                              if lm /= DOWN && checkInList (x', yD) coords
                                then goDown
                                else game

    c10 =
      if lm /= RIGHT && checkInList (xR, y') coords
        then goRight
        else
          if lm /= UP && checkInList (x', yU) coords
            then goUp
            else
              if lm /= LEFT && checkInList (xL, y') coords
                then goLeft
                else
                  if lm /= DOWN && checkInList (x', yD) coords
                    then goDown
                    else game

    cWalls = findWalls game 4

    goRight =
      game
        { clyde =
            Ghost
              { gName = Clyde
              , gSpeed = (3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = LEFT
              , gDirection = RIGHT
              }
        }
    goLeft =
      game
        { clyde =
            Ghost
              { gName = Clyde
              , gSpeed = (-3.7, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = RIGHT
              , gDirection = LEFT
              }
        }
    goUp =
      game
        { clyde =
            Ghost
              { gName = Clyde
              , gSpeed = (0, 3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = DOWN
              , gDirection = UP
              }
        }
    goDown =
      game
        { clyde =
            Ghost
              { gName = Clyde
              , gSpeed = (0, -3.7)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = UP
              , gDirection = DOWN
              }
        }

    testUp =
      if elem (x', yU) coords
        then
          game
            { clyde =
                Ghost
                  { gName = Clyde
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testDown =
      if elem (x', yD) coords
        then
          game
            { clyde =
                Ghost
                  { gName = Clyde
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testLeft =
      if elem (xL, y') coords
        then
          game
            { clyde =
                Ghost
                  { gName = Clyde
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    testRight =
      if elem (xR, y') coords
        then
          game
            { clyde =
                Ghost
                  { gName = Clyde
                  , gSpeed = (0, 0)
                  , gLocation = (x', y')
                  , gTarget = curTarget
                  , gLastMove = lm
                  , gDirection = cD
                  }
            }
        else game
    stop =
      game
        { clyde =
            Ghost
              { gName = Clyde
              , gSpeed = (0, 0)
              , gLocation = (x', y')
              , gTarget = curTarget
              , gLastMove = lm
              , gDirection = cD
              }
        }


-- Wall configuration codes:
-- 1 = all 4 sides open
-- 2 = left, up, right open (down wall)
-- 3 = up, right, down open (left wall)
-- 4 = right, down, left open (up wall)
-- 5 = down, left, up open (right wall)
-- 10 = fallback
findWalls :: PacGame -> Int -> Int
findWalls game x = case x of
  1 -> checkBlinky
  2 -> checkPinky
  3 -> checkInky
  4 -> checkClyde
  _ -> 10
  where
    --- Blinky ---
    (bx, by) = game.blinky.gLocation
    bx' = roundF bx
    by' = roundF by
    bU = by' + 1
    bD = by' - 1
    bL = bx' - 1
    bR = bx' + 1
    checkBlinky =
      if checkInList (bL, by') coords
        && checkInList (bR, by') coords
        && checkInList (bx', bU) coords
        && checkInList (bx', bD) coords
        then 1
        else
          if checkInList (bL, by') coords
            && checkInList (bR, by') coords
            && checkInList (bx', bU) coords
            && elem (bx', bD) coords
            then 2
            else
              if elem (bL, by') coords
                && checkInList (bR, by') coords
                && checkInList (bx', bU) coords
                && checkInList (bx', bD) coords
                then 3
                else
                  if checkInList (bL, by') coords
                    && checkInList (bR, by') coords
                    && elem (bx', bU) coords
                    && checkInList (bx', bD) coords
                    then 4
                    else
                      if checkInList (bL, by') coords
                        && elem (bR, by') coords
                        && checkInList (bx', bU) coords
                        && checkInList (bx', bD) coords
                        then 5
                        else 10

    --- Pinky ---
    (px, py) = game.pinky.gLocation
    px' = roundF px
    py' = roundF py
    pU = py' + 1
    pD = py' - 1
    pL = px' - 1
    pR = px' + 1
    checkPinky =
      if checkInList (pL, py') coords
        && checkInList (pR, py') coords
        && checkInList (px', pU) coords
        && checkInList (px', pD) coords
        then 1
        else
          if checkInList (pL, py') coords
            && checkInList (pR, py') coords
            && checkInList (px', pU) coords
            && elem (px', pD) coords
            then 2
            else
              if elem (pL, py') coords
                && checkInList (pR, py') coords
                && checkInList (px', pU) coords
                && checkInList (px', pD) coords
                then 3
                else
                  if checkInList (pL, py') coords
                    && checkInList (pR, py') coords
                    && elem (px', pU) coords
                    && checkInList (px', pD) coords
                    then 4
                    else
                      if checkInList (pL, py') coords
                        && elem (pR, py') coords
                        && checkInList (px', pU) coords
                        && checkInList (px', pD) coords
                        then 5
                        else 10

    --- Inky ---
    (ix, iy) = game.inky.gLocation
    ix' = roundF ix
    iy' = roundF iy
    iU = iy' + 1
    iD = iy' - 1
    iL = ix' - 1
    iR = ix' + 1
    checkInky =
      if checkInList (iL, iy') coords
        && checkInList (iR, iy') coords
        && checkInList (ix', iU) coords
        && checkInList (ix', iD) coords
        then 1
        else
          if checkInList (iL, iy') coords
            && checkInList (iR, iy') coords
            && checkInList (ix', iU) coords
            && elem (ix', iD) coords
            then 2
            else
              if elem (iL, iy') coords
                && checkInList (iR, iy') coords
                && checkInList (ix', iU) coords
                && checkInList (ix', iD) coords
                then 3
                else
                  if checkInList (iL, iy') coords
                    && checkInList (iR, iy') coords
                    && elem (ix', iU) coords
                    && checkInList (ix', iD) coords
                    then 4
                    else
                      if checkInList (iL, iy') coords
                        && elem (iR, iy') coords
                        && checkInList (ix', iU) coords
                        && checkInList (ix', iD) coords
                        then 5
                        else 10

    --- Clyde ---
    (cx, cy) = game.clyde.gLocation
    cx' = roundF cx
    cy' = roundF cy
    cU = cy' + 1
    cD' = cy' - 1
    cL = cx' - 1
    cR = cx' + 1
    checkClyde =
      if checkInList (cL, cy') coords
        && checkInList (cR, cy') coords
        && checkInList (cx', cU) coords
        && checkInList (cx', cD') coords
        then 1
        else
          if checkInList (cL, cy') coords
            && checkInList (cR, cy') coords
            && checkInList (cx', cU) coords
            && elem (cx', cD') coords
            then 2
            else
              if elem (cL, cy') coords
                && checkInList (cR, cy') coords
                && checkInList (cx', cU) coords
                && checkInList (cx', cD') coords
                then 3
                else
                  if checkInList (cL, cy') coords
                    && checkInList (cR, cy') coords
                    && elem (cx', cU) coords
                    && checkInList (cx', cD') coords
                    then 4
                    else
                      if checkInList (cL, cy') coords
                        && elem (cR, cy') coords
                        && checkInList (cx', cU) coords
                        && checkInList (cx', cD') coords
                        then 5
                        else 10
