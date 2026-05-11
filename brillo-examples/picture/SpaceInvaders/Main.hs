{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brillo
import Brillo.Data.Point (pointInBox)
import Brillo.Interface.Pure.Game
import Data.Function ((&))
import Data.List (partition)
import Data.List qualified as List
import Data.Text qualified as T


-- Constants

windowWidth, windowHeight :: Float
windowWidth = 640
windowHeight = 480


playerY :: Float
playerY = -200


playerSpeed :: Float
playerSpeed = 200


bulletSpeed :: Float
bulletSpeed = 300


alienBulletSpeed :: Float
alienBulletSpeed = 150


alienStepX :: Float
alienStepX = 20


alienStepY :: Float
alienStepY = 20


alienMoveInterval :: Float
alienMoveInterval = 0.6


alienShootInterval :: Float
alienShootInterval = 1.2


cols :: Int
cols = 11


rows :: Int
rows = 5


totalAliens :: Int
totalAliens = cols * rows


-- Types

data Alien = Octopus | Crab | Squid


data Direction = DirLeft | DirRight


flipDir :: Direction -> Direction
flipDir DirLeft = DirRight
flipDir DirRight = DirLeft


dirSign :: Direction -> Float
dirSign DirLeft = -1
dirSign DirRight = 1


data AlienInfo = AlienInfo
  { alienType :: Alien
  , alienPos :: (Float, Float)
  , alive :: Bool
  }


data Bullet = Bullet
  { bulletPos :: (Float, Float)
  }


data Shield = Shield
  { shieldPos :: (Float, Float)
  , shieldHealth :: Int
  }


data Phase = WaitingToStart | Playing | Won | Dead
  deriving (Eq)


data Keys = Keys
  { leftHeld :: Bool
  , rightHeld :: Bool
  }


data GameState = GameState
  { playerX :: Float
  , playerBullet :: Maybe Bullet
  , alienBullets :: [Bullet]
  , aliens :: [AlienInfo]
  , aliveCount :: Int
  , shields :: [Shield]
  , alienDir :: Direction
  , alienTimer :: Float
  , alienShootTimer :: Float
  , score :: Int
  , lives :: Int
  , phase :: Phase
  , moveDown :: Bool
  , keysHeld :: Keys
  }


-- Alien scoring

alienToPoints :: Alien -> Int
alienToPoints = \case
  Octopus -> 10
  Crab -> 20
  Squid -> 30


-- Initial state

makeAlienGrid :: [AlienInfo]
makeAlienGrid =
  [ AlienInfo
      { alienType = alienForRow r
      , alienPos = (fromIntegral c * 40 - 200, fromIntegral r * 35 + 40)
      , alive = True
      }
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1]
  ]
  where
    alienForRow r
      | r < 2 = Octopus
      | r < 4 = Crab
      | otherwise = Squid


makeShields :: [Shield]
makeShields =
  [ Shield (x, -140) 4
  | x <- [-180, -60, 60, 180]
  ]


initialState :: GameState
initialState =
  GameState
    { playerX = 0
    , playerBullet = Nothing
    , alienBullets = []
    , aliens = makeAlienGrid
    , aliveCount = totalAliens
    , shields = makeShields
    , alienDir = DirRight
    , alienTimer = 0
    , alienShootTimer = 0
    , score = 0
    , lives = 3
    , phase = WaitingToStart
    , moveDown = False
    , keysHeld = Keys False False
    }


-- Rendering

render :: GameState -> Picture
render gs =
  Pictures $
    concat
      [ [drawPlayer gs.playerX]
      , map drawAlienInfo (filter (.alive) gs.aliens)
      , maybe [] (\b -> [drawBullet green b]) gs.playerBullet
      , map (drawBullet white) gs.alienBullets
      , map drawShield gs.shields
      , [drawLabel (-300) "SCORE: " gs.score, drawLabel 200 "LIVES: " gs.lives]
      , overlay
      ]
  where
    drawPlayer :: Float -> Picture
    drawPlayer px =
      Color green $
        Translate px playerY $
          Pictures
            [ rectangleSolid 30 10
            , Translate 0 7 $ rectangleSolid 6 10
            ]

    drawAlienInfo :: AlienInfo -> Picture
    drawAlienInfo ai =
      let (ax, ay) = ai.alienPos
      in  Color (alienColor ai.alienType) $
            Translate ax ay $
              drawAlien ai.alienType

    drawAlien :: Alien -> Picture
    drawAlien Octopus =
      Pictures
        [ rectangleSolid 16 12
        , Translate (-8) (-6) $ rectangleSolid 4 6
        , Translate 8 (-6) $ rectangleSolid 4 6
        ]
    drawAlien Crab =
      Pictures
        [ rectangleSolid 20 10
        , Translate (-10) 2 $ rectangleSolid 4 8
        , Translate 10 2 $ rectangleSolid 4 8
        ]
    drawAlien Squid =
      Pictures
        [ rectangleSolid 14 14
        , Translate 0 10 $ rectangleSolid 6 6
        ]

    alienColor :: Alien -> Color
    alienColor Octopus = cyan
    alienColor Crab = white
    alienColor Squid = magenta

    drawBullet :: Color -> Bullet -> Picture
    drawBullet clr b =
      let (bx, by) = b.bulletPos
      in  Color clr $ Translate bx by $ rectangleSolid 2 8

    drawShield :: Shield -> Picture
    drawShield s =
      let (sx, sy) = s.shieldPos
          intensity = fromIntegral s.shieldHealth / 4.0
      in  Color (makeColor 0 intensity 0 1) $
            Translate sx sy $
              rectangleSolid 40 20

    drawLabel :: Float -> String -> Int -> Picture
    drawLabel tx label n =
      Color white $
        Translate tx 220 $
          Scale 0.15 0.15 $
            Text $
              T.pack (label ++ show n)

    overlay = case gs.phase of
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
      Won ->
        [ Color green $
            Translate (-160) 0 $
              Scale 0.5 0.5 $
                Text "YOU WIN!"
        , Color white $
            Translate (-175) (-50) $
              Scale 0.2 0.2 $
                Text "Press SPACE to play again."
        ]
      Playing -> []


-- Input handling

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = case gs.phase of
  WaitingToStart -> gs{phase = Playing}
  Dead -> initialState{phase = Playing}
  Won -> initialState{phase = Playing}
  Playing -> shoot gs
handleInput (EventKey (SpecialKey KeyLeft) ks _ _) gs =
  gs{keysHeld = (gs.keysHeld){leftHeld = ks == Down}}
handleInput (EventKey (SpecialKey KeyRight) ks _ _) gs =
  gs{keysHeld = (gs.keysHeld){rightHeld = ks == Down}}
handleInput _ gs = gs


shoot :: GameState -> GameState
shoot gs = case gs.playerBullet of
  -- Classic Space Invaders only allows one player bullet on screen
  Just _ -> gs
  Nothing ->
    gs{playerBullet = Just Bullet{bulletPos = (gs.playerX, playerY + 15)}}


-- Update

update :: Float -> GameState -> GameState
update dt gs
  | gs.phase /= Playing = gs
  | otherwise =
      gs
        & movePlayer dt
        & movePlayerBullet dt
        & moveAlienBullets dt
        & stepAliens dt
        & alienShooting dt
        & checkCollisions
        & checkWinLose


movePlayer :: Float -> GameState -> GameState
movePlayer dt gs =
  let Keys l r = gs.keysHeld
      dx
        | l && not r = -playerSpeed * dt
        | r && not l = playerSpeed * dt
        | otherwise = 0
  in  gs{playerX = clamp (-280) 280 (gs.playerX + dx)}


clamp :: Float -> Float -> Float -> Float
clamp lo hi x = max lo (min hi x)


movePlayerBullet :: Float -> GameState -> GameState
movePlayerBullet dt gs = case gs.playerBullet of
  Nothing -> gs
  Just b ->
    let (bx, by) = b.bulletPos
        newY = by + bulletSpeed * dt
    in  if newY > windowHeight / 2
          then gs{playerBullet = Nothing}
          else gs{playerBullet = Just b{bulletPos = (bx, newY)}}


moveAlienBullets :: Float -> GameState -> GameState
moveAlienBullets dt gs =
  let lowerBound = -(windowHeight / 2)
      step b =
        let (bx, by) = b.bulletPos
        in  b{bulletPos = (bx, by - alienBulletSpeed * dt)}
      keep b = snd b.bulletPos > lowerBound
  in  gs{alienBullets = filter keep (map step gs.alienBullets)}


stepAliens :: Float -> GameState -> GameState
stepAliens dt gs
  | newTimer < currentInterval gs = gs{alienTimer = newTimer}
  | gs.moveDown = moveAliensDown gs'
  | otherwise = moveAliensSideways gs'
  where
    newTimer = gs.alienTimer + dt
    gs' = gs{alienTimer = 0}


-- Speed up as aliens are destroyed
currentInterval :: GameState -> Float
currentInterval gs =
  let ratio = fromIntegral gs.aliveCount / fromIntegral totalAliens
  in  max 0.05 (alienMoveInterval * ratio)


mapAlive :: ((Float, Float) -> (Float, Float)) -> [AlienInfo] -> [AlienInfo]
mapAlive f = map (\a -> if a.alive then a{alienPos = f a.alienPos} else a)


moveAliensSideways :: GameState -> GameState
moveAliensSideways gs =
  let dx = alienStepX * dirSign gs.alienDir
      moved = mapAlive (\(x, y) -> (x + dx, y)) gs.aliens
      extent (lo, hi) a
        | not a.alive = (lo, hi)
        | otherwise = let x = fst a.alienPos in (min lo x, max hi x)
      (minX, maxX) = List.foldl' extent (1 / 0, -(1 / 0)) moved
      hitEdge = maxX > 280 || minX < -280
  in  gs{aliens = moved, moveDown = hitEdge}


moveAliensDown :: GameState -> GameState
moveAliensDown gs =
  gs
    { aliens = mapAlive (\(x, y) -> (x, y - alienStepY)) gs.aliens
    , alienDir = flipDir gs.alienDir
    , moveDown = False
    }


alienShooting :: Float -> GameState -> GameState
alienShooting dt gs
  | newTimer < alienShootInterval = gs{alienShootTimer = newTimer}
  | null aliveAliens = gs{alienShootTimer = 0}
  | otherwise =
      let shooterIdx = floor (newTimer * 1000) `mod` length aliveAliens
          shooter = aliveAliens !! shooterIdx
          (sx, sy) = shooter.alienPos
          newBullet = Bullet{bulletPos = (sx, sy - 15)}
      in  gs
            { alienBullets = newBullet : gs.alienBullets
            , alienShootTimer = 0
            }
  where
    newTimer = gs.alienShootTimer + dt
    aliveAliens = filter (.alive) gs.aliens


checkCollisions :: GameState -> GameState
checkCollisions gs =
  gs
    & playerBulletHitsAliens
    & alienBulletsHitPlayer
    & bulletsHitShields


playerBulletHitsAliens :: GameState -> GameState
playerBulletHitsAliens gs = case gs.playerBullet of
  Nothing -> gs
  Just b ->
    let (hit, newAliens, points) = checkAlienHits b gs.aliens
    in  if hit
          then
            gs
              { playerBullet = Nothing
              , aliens = newAliens
              , aliveCount = gs.aliveCount - 1
              , score = gs.score + points
              }
          else gs


checkAlienHits :: Bullet -> [AlienInfo] -> (Bool, [AlienInfo], Int)
checkAlienHits _ [] = (False, [], 0)
checkAlienHits b (a : as)
  | a.alive && bulletHits 12 10 b a.alienPos =
      (True, a{alive = False} : as, alienToPoints a.alienType)
  | otherwise =
      let (hit, rest, pts) = checkAlienHits b as
      in  (hit, a : rest, pts)


bulletHits :: Float -> Float -> Bullet -> (Float, Float) -> Bool
bulletHits hw hh b (cx, cy) =
  pointInBox b.bulletPos (cx - hw, cy - hh) (cx + hw, cy + hh)


alienBulletsHitPlayer :: GameState -> GameState
alienBulletsHitPlayer gs =
  let playerPos = (gs.playerX, playerY)
      (hitBullets, safeBullets) =
        partition (\b -> bulletHits 15 10 b playerPos) gs.alienBullets
  in  if null hitBullets
        then gs
        else gs{alienBullets = safeBullets, lives = gs.lives - 1}


bulletsHitShields :: GameState -> GameState
bulletsHitShields gs =
  let (pb', shields1) = case gs.playerBullet of
        Nothing -> (Nothing, gs.shields)
        Just b -> checkBulletShields b gs.shields
      (alienBullets', shields2) =
        foldr checkAlienBullet ([], shields1) gs.alienBullets
  in  gs{playerBullet = pb', alienBullets = alienBullets', shields = shields2}
  where
    checkBulletShields :: Bullet -> [Shield] -> (Maybe Bullet, [Shield])
    checkBulletShields b [] = (Just b, [])
    checkBulletShields b (s : ss)
      | s.shieldHealth > 0 && bulletHits 22 12 b s.shieldPos =
          (Nothing, s{shieldHealth = s.shieldHealth - 1} : ss)
      | otherwise =
          let (b', ss') = checkBulletShields b ss
          in  (b', s : ss')

    checkAlienBullet :: Bullet -> ([Bullet], [Shield]) -> ([Bullet], [Shield])
    checkAlienBullet b (bs, ss) =
      case checkBulletShields b ss of
        (Just b', ss') -> (b' : bs, ss')
        (Nothing, ss') -> (bs, ss')


checkWinLose :: GameState -> GameState
checkWinLose gs
  | gs.lives <= 0 = gs{phase = Dead}
  | gs.aliveCount == 0 = gs{phase = Won}
  | any alienTooLow gs.aliens = gs{phase = Dead}
  | otherwise = gs
  where
    alienTooLow a = a.alive && snd a.alienPos < playerY + 30


main :: IO ()
main =
  play
    (InWindow "Space Invaders" (round windowWidth, round windowHeight) (100, 100))
    black
    60
    initialState
    render
    handleInput
    update
