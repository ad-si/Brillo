{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brillo
import Brillo.Interface.Pure.Game
import Data.Fixed (mod')
import Data.Text qualified as T

import GhostAI (
  ghostCollisionSwitch,
  ghostModeSwitch,
  listLength,
  moveToTargetB,
  moveToTargetC,
  moveToTargetI,
  moveToTargetP,
  releaseClyde,
  releaseInky,
  releasePinky,
  roundF,
 )
import Maze
import Structs


handleKeys :: Event -> PacGame -> PacGame
handleKeys event game
  | curTime < 3 = game
  | otherwise = case event of
      EventKey (SpecialKey KeyUp) _ _ _ -> game{bufDirection = UP}
      EventKey (SpecialKey KeyDown) _ _ _ -> game{bufDirection = DOWN}
      EventKey (SpecialKey KeyLeft) _ _ _ -> game{bufDirection = LEFT}
      EventKey (SpecialKey KeyRight) _ _ _ -> game{bufDirection = RIGHT}
      EventKey (Char 'w') _ _ _ -> game{bufDirection = UP}
      EventKey (Char 's') _ _ _ -> game{bufDirection = DOWN}
      EventKey (Char 'a') _ _ _ -> game{bufDirection = LEFT}
      EventKey (Char 'd') _ _ _ -> game{bufDirection = RIGHT}
      _ -> game
  where
    curTime = game.time


initialize :: PacGame
initialize =
  Game
    { lives = 3
    , gameStatus = PLAYING
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
    , time = 0.0
    , score = 0
    , pellets = pelletsL
    , direction = LEFT
    , bufDirection = STOP
    , gMode = SCATTER
    , lcrB = (0, 0)
    , lcrP = (0, 0)
    , lcrI = (0, 0)
    , lcrC = (0, 0)
    , pPellets = powerPellet
    , fTime = 0.0
    }


beginTimer :: PacGame -> PacGame
beginTimer game = game{time = curTime + 1 / fromIntegral fps}
  where
    curTime = game.time


renderPacman :: PacGame -> Picture
renderPacman game =
  Translate (x * 30) (y * 30) $
    Rotate rot $
      Color yellow $
        arcSolid mouth (360 - mouth) 13
  where
    (x, y) = game.pacman.location
    mouth = 5 + 40 * abs (sin (game.time * 8))
    rot = case game.direction of
      RIGHT -> 0
      UP -> -90
      LEFT -> 180
      DOWN -> 90
      STOP -> 0


renderGhost :: Color -> Point -> Picture
renderGhost clr (x, y) =
  Translate (x * 30) (y * 30) $ Color clr $ circleSolid 13


renderPinky :: PacGame -> Picture
renderPinky game
  | mode == CHASE || mode == SCATTER = renderGhost (light rose) pos
  | target == (0, 0) = renderGhost (dark white) pos
  | otherwise = renderGhost blue pos
  where
    pos = game.pinky.gLocation
    mode = game.gMode
    target = game.pinky.gTarget


renderInky :: PacGame -> Picture
renderInky game
  | mode == CHASE || mode == SCATTER = renderGhost cyan pos
  | target == (0, 0) = renderGhost (dark white) pos
  | otherwise = renderGhost blue pos
  where
    pos = game.inky.gLocation
    mode = game.gMode
    target = game.inky.gTarget


renderBlinky :: PacGame -> Picture
renderBlinky game
  | mode == CHASE || mode == SCATTER = renderGhost red pos
  | target == (0, 0) = renderGhost (dark white) pos
  | otherwise = renderGhost blue pos
  where
    pos = game.blinky.gLocation
    mode = game.gMode
    target = game.blinky.gTarget


renderClyde :: PacGame -> Picture
renderClyde game
  | mode == CHASE || mode == SCATTER = renderGhost orange pos
  | target == (0, 0) = renderGhost (dark white) pos
  | otherwise = renderGhost blue pos
  where
    pos = game.clyde.gLocation
    mode = game.gMode
    target = game.clyde.gTarget


renderStuff :: PacGame -> Picture
renderStuff game
  | curTime <= 3 =
      Translate (-30) (-65) $ Scale 0.15 0.15 $ Color yellow $ Text "Ready!"
  | otherwise =
      Color white $
        Scale 0.2 0.2 $
          Pictures [rScore, rLives, modeText]
  where
    rScore = Translate (-1400) 1475 $ Text $ T.pack $ "Score: " ++ show game.score
    rLives = Translate (-1800) (-1880) $ Text "Lives: "
    modeText = Translate 200 1475 $ Text $ T.pack $ "Mode: " ++ show game.gMode
    curTime = game.time


renderPellets :: PacGame -> Picture
renderPellets game =
  Pictures
    [ Translate (x * 30) (y * 30) $ Color white $ circleSolid 4
    | (x, y) <- game.pellets
    ]


renderPowerPellets :: PacGame -> Picture
renderPowerPellets game
  | mod' curTime 0.3 >= 0 && mod' curTime 0.3 <= 0.15 =
      Pictures
        [ Translate (x * 30) (y * 30) $ Color white $ circleSolid 8
        | (x, y) <- game.pPellets
        ]
  | otherwise = Blank
  where
    curTime = game.time


renderLives :: PacGame -> Picture
renderLives game
  | curTime <= 3 = Blank
  | curLives >= 3 = Pictures [renderOne, renderTwo, renderThree]
  | curLives == 2 = Pictures [renderOne, renderTwo]
  | curLives == 1 = Pictures [renderOne]
  | otherwise = Blank
  where
    curTime = game.time
    curLives = game.lives
    renderOne = Translate (-270) (-370) $ Color yellow $ circleSolid 13
    renderTwo = Translate (-235) (-370) $ Color yellow $ circleSolid 13
    renderThree = Translate (-200) (-370) $ Color yellow $ circleSolid 13


displayWindow :: Display
displayWindow = InWindow "Pac-Man" (800, 800) (350, 350)


renderVictory :: Picture
renderVictory =
  Translate (-250) 0 $
    Scale 0.3 0.3 $
      Color white $
        Text "Congratulations! You Win!"


renderDefeat :: Picture
renderDefeat =
  Translate (-170) 0 $ Scale 0.4 0.4 $ Color white $ Text "Game Over"


render :: PacGame -> Picture
render game = case game.gameStatus of
  WON -> renderVictory
  LOST -> renderDefeat
  PLAYING ->
    Pictures
      [ renderLives game
      , renderPowerPellets game
      , renderW coords
      , renderPacman game
      , renderStuff game
      , renderPellets game
      , renderPinky game
      , renderInky game
      , renderBlinky game
      , renderClyde game
      ]


main :: IO ()
main = do
  play displayWindow black fps initialize render handleKeys step
  where
    step :: Float -> PacGame -> PacGame
    step sec game =
      sMove sec $
        releaseInky $
          releasePinky $
            releaseClyde $
              moveToTargetC $
                moveToTargetI $
                  moveToTargetP $
                    moveToTargetB $
                      ghostModeSwitch $
                        beginTimer $
                          ghostCollisionSwitch $
                            pacScore $
                              pacEat $
                                pacEatP $
                                  wallCollision $
                                    execM $
                                      checkTeleport game


sMove :: Float -> PacGame -> PacGame
sMove sec game =
  game
    { pacman =
        Characters
          { cName = Pacman
          , location = (x + vx * sec, y + vy * sec)
          , speed = (vx, vy)
          }
    , blinky = game.blinky{gLocation = (bx + bvx * sec, by + bvy * sec)}
    , pinky = game.pinky{gLocation = (px + pvx * sec, py + pvy * sec)}
    , inky = game.inky{gLocation = (ix + ivx * sec, iy + ivy * sec)}
    , clyde = game.clyde{gLocation = (cx + cvx * sec, cy + cvy * sec)}
    }
  where
    (x, y) = game.pacman.location
    (vx, vy) = game.pacman.speed
    (bx, by) = game.blinky.gLocation
    (bvx, bvy) = game.blinky.gSpeed
    (px, py) = game.pinky.gLocation
    (pvx, pvy) = game.pinky.gSpeed
    (ix, iy) = game.inky.gLocation
    (ivx, ivy) = game.inky.gSpeed
    (cx, cy) = game.clyde.gLocation
    (cvx, cvy) = game.clyde.gSpeed


execM :: PacGame -> PacGame
execM game
  | curTime <= 2.5 = game
  | game.direction == game.bufDirection = game
  | game.direction /= game.bufDirection = newgame
  | otherwise = game
  where
    curTime = game.time
    newgame
      | (vx, vy) == (0, 0) = case game.bufDirection of
          UP -> up
          DOWN -> down
          LEFT -> left
          RIGHT -> right
          STOP -> stop
      | (vx, vy) /= (0, 0) = case game.bufDirection of
          UP -> tUP
          DOWN -> tDOWN
          LEFT -> tLEFT
          RIGHT -> tRIGHT
          STOP -> tSTOP
      | otherwise = game
      where
        (x, y) = game.pacman.location
        (vx, vy) = game.pacman.speed
        up =
          game
            { direction = UP
            , pacman = Characters{cName = Pacman, speed = (0, 4), location = (x, y)}
            }
        down =
          game
            { direction = DOWN
            , pacman = Characters{cName = Pacman, speed = (0, -4), location = (x, y)}
            }
        left =
          game
            { direction = LEFT
            , pacman = Characters{cName = Pacman, speed = (-4, 0), location = (x, y)}
            }
        right =
          game
            { direction = RIGHT
            , pacman = Characters{cName = Pacman, speed = (4, 0), location = (x, y)}
            }
        stop =
          game
            { direction = STOP
            , pacman = Characters{cName = Pacman, speed = (0, 0), location = (x, y)}
            }
        x' = roundF x
        y' = roundF y
        tUP =
          if elem (x', y' + 1) coords
            then game
            else
              game
                { direction = UP
                , pacman = Characters{cName = Pacman, speed = (0, 4), location = (x', y')}
                }
        tDOWN =
          if elem (x', y' - 1) coords
            then game
            else
              game
                { direction = DOWN
                , pacman = Characters{cName = Pacman, speed = (0, -4), location = (x', y')}
                }
        tLEFT =
          if elem (x' - 1, y') coords
            then game
            else
              game
                { direction = LEFT
                , pacman = Characters{cName = Pacman, speed = (-4, 0), location = (x', y')}
                }
        tRIGHT =
          if elem (x' + 1, y') coords
            then game
            else
              game
                { direction = RIGHT
                , pacman = Characters{cName = Pacman, speed = (4, 0), location = (x', y')}
                }
        tSTOP = game


wallCollision :: PacGame -> PacGame
wallCollision game = case game.direction of
  UP -> up
  DOWN -> down
  LEFT -> left
  RIGHT -> right
  STOP -> stop
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    nearGrid = abs (x - x') <= 0.1 && abs (y - y') <= 0.1
    stopped =
      game
        { direction = STOP
        , pacman = Characters{cName = Pacman, speed = (0, 0), location = (x', y')}
        }
    up = if nearGrid && elem (x', y' + 1) coords then stopped else game
    down = if nearGrid && elem (x', y' - 1) coords then stopped else game
    left = if nearGrid && elem (x' - 1, y') coords then stopped else game
    right = if nearGrid && elem (x' + 1, y') coords then stopped else game
    stop = stopped


pacEat :: PacGame -> PacGame
pacEat game
  | (x', y') `elem` pelletsL =
      game{pellets = [p | p <- curPellets, p /= (x', y')]}
  | otherwise = game
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    curPellets = game.pellets


pacEatP :: PacGame -> PacGame
pacEatP game
  | (x', y') `elem` pPelletsL =
      game
        { fTime = 0
        , gMode = FRIGHTENED
        , pPellets = [p | p <- curPellets, p /= (x', y')]
        }
  | otherwise = game
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    curPellets = game.pPellets
    pPelletsL = game.pPellets


pacScore :: PacGame -> PacGame
pacScore game
  | curScore >= 1700 = game{gameStatus = WON}
  | otherwise = game{score = newscore}
  where
    newscore =
      (listLength pelletsL - listLength curPellets) * 10
        + (listLength powerPellet - listLength curPowerPellets) * 50
    curPellets = game.pellets
    curScore = game.score
    curPowerPellets = game.pPellets


checkTeleport :: PacGame -> PacGame
checkTeleport game
  | (x', y') == (-10, 0) =
      game{pacman = Characters{cName = Pacman, speed = curSpeed, location = (9, 0)}}
  | (x', y') == (10, 0) =
      game{pacman = Characters{cName = Pacman, speed = curSpeed, location = (-9, 0)}}
  | otherwise = game
  where
    (x, y) = game.pacman.location
    x' = roundF x
    y' = roundF y
    curSpeed = game.pacman.speed
