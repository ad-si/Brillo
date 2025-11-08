{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import Brillo
import Brillo.Data.Color
import Brillo.Interface.Pure.Game
import Control.Lens (element, set)
import Data.List (reverse, transpose)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import System.Random (StdGen, getStdGen, random, randoms)


--------------------------------------------
-- TODO:
-- loss detection
-- movement animations
-- aesthetic improvements
-------------------------------------------

main =
  do
    g <- getStdGen
    play
      ( InWindow
          "2048 in Haskell by Maia"
          (410, 500)
          (20, 20)
      )
      (makeColorI 193 177 156 255) -- background color
      fps -- simulation steps per second
      (initPositions g) -- initial world
      drawWorld -- function to convert world to picture
      handleInputEvents -- TODO add randomness
      stepWorld


fps = 20


data Tile = Tile {val :: Int, popInTime :: Float, popOutTime :: Float}
  deriving (Eq, Show)
type Row = [Tile]
data World = World {board :: [[Tile]], gen :: StdGen, score :: Int}


instance Eq World where
  x == y = board x == board y && score x == score y


--------------------------------------------
-- Utils for world generation and updating
--------------------------------------------

tileIntFunction :: (Int -> Int) -> Tile -> Tile
tileIntFunction f t = t{val = f (val t)}


setVal :: Tile -> Int -> Tile
setVal t i = t{val = i}


setVals :: [Tile] -> [Int] -> [Tile]
setVals ts is = zipWith setVal ts is


setValss :: [[Tile]] -> [[Int]] -> [[Tile]]
setValss tss iss = zipWith setVals tss iss


---------------------------------------------
-- Initial world generation
---------------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0


makeTile :: Int -> Tile
makeTile i = Tile{val = i, popInTime = 0.0, popOutTime = 0.0}


-- generates an initial world state with, on average, two 2s to start
initPositions :: StdGen -> World
initPositions g =
  let origBoard = chunksOf 4 $ map makeTile $ replicate 16 0
  in  addTile $ addTile World{board = origBoard, gen = g, score = 0}


--------------------------------------------
-- Regeneration of 2s
--------------------------------------------

replaceNthZero :: Int -> Int -> [Tile] -> [Tile]
replaceNthZero v _ [] = []
replaceNthZero v 0 (x : xs) =
  if val x == 0
    then (Tile{val = v, popInTime = 0.5, popOutTime = 0.0} : xs)
    else x : (replaceNthZero v 0 xs)
replaceNthZero v n (x : xs) =
  if val x == 0
    then x : (replaceNthZero v (n - 1) xs)
    else x : (replaceNthZero v n xs)


reciprocalOddsOf4 = 10


addTile :: World -> World
addTile world =
  let numZeros = (sum . (map toInt) . (map (== 0)) . map val . concat) (board world)
      (rand, newGen) = random (gen world) :: (Int, StdGen)
      n = if numZeros == 0 then 0 else rand `mod` numZeros
      (rand2, newGen2) = random newGen :: (Int, StdGen)
      v = if rand2 `mod` reciprocalOddsOf4 == 0 then 4 else 2
      newBoard = chunksOf 4 $ replaceNthZero v n $ concat (board world)
  in  world{board = newBoard, gen = newGen}


---------------------------------------------
-- Input handling
-- -----------------------------------------
--
data Direction = U | D | L | R


keyDir :: Key -> Maybe Direction
keyDir (SpecialKey KeyUp) = Just U
keyDir (SpecialKey KeyDown) = Just D
keyDir (SpecialKey KeyLeft) = Just L
keyDir (SpecialKey KeyRight) = Just R
keyDir _ = Nothing


handleInputEvents :: Event -> World -> World
handleInputEvents (EventKey k Down _ _) world =
  let dir = keyDir k
      newWorld = go dir world
  in  if newWorld == world
        then world
        else addTile newWorld
handleInputEvents _ x = x


--------------------------------------------------------
-- Time steps: All they do is update the animations   --
-- -----------------------------------------------------

popInSpeed = 4
popOutSpeed = 0.5


-- changes animation times for each tile
updateTile :: Float -> Tile -> Tile
updateTile dt t =
  if val t > 0
    then
      t
        { popInTime =
            if popInTime t - dt * popInSpeed > 0 then popInTime t - dt * popInSpeed else 0
        , popOutTime =
            if popOutTime t - dt * popInSpeed > 0 then popOutTime t - dt * popInSpeed else 0
        }
    else t{popInTime = 0, popOutTime = 0}


updateTiles :: Float -> [[Tile]] -> [[Tile]]
updateTiles dt tss = (map (map (updateTile dt))) tss


stepWorld :: Float -> World -> World
stepWorld dt world = world{board = updateTiles dt (board world)}


---------------------------------------------------------
--  Drawing and display
---------------------------------------------------------

rowHgt = 100


gameOverMessage :: Picture
gameOverMessage =
  pictures
    [ translate (-500) (-500) $ color translucentWhite $ rectangleSolid 2000 2000
    , translate (-335) (-150) $
        scale 0.5 0.5 $
          color black $
            text (T.pack "Game Over")
    ]
  where
    translucentWhite = makeColorI 255 255 255 150


drawWorld :: World -> Picture
drawWorld w@World{board = [r1, r2, r3, r4], score = s} =
  translate (150) (150) $
    pictures $
      [ drawRow r1
      , translate 0 (-rowHgt) (drawRow r2)
      , translate 0 (-rowHgt * 2) (drawRow r3)
      , translate 0 (-rowHgt * 3) (drawRow r4)
      , translate (-300) 60 $
          scale 0.2 0.2 $
            color white $
              text $
                T.pack $
                  "Score: " ++ (show s)
      ]
        ++ gameOverPicture
  where
    gameOverPicture = if lost then [gameOverMessage] else []
    lost =
      go (Just R) w == w
        && go (Just L) w == w
        && go (Just U) w == w
        && go (Just D) w == w


-- debugPicture ])

tileS = 90
textScale = 0.2


-- TODO: also make a zipper to *pictures* to also adjust number position, text color, etc. by number

colorZipper :: [(Int, Color)]
colorZipper =
  [ (2, makeColorI 238 228 218 255)
  , (4, makeColorI 237 224 200 255)
  , (8, makeColorI 242 177 121 255)
  , (16, makeColorI 245 149 99 255)
  , (32, makeColorI 246 124 95 255)
  , (64, makeColorI 246 102 62 255)
  , (128, makeColorI 238 208 114 255)
  , (256, makeColorI 237 204 97 255)
  , (512, makeColorI 237 200 80 255)
  , (1024, makeColorI 237 197 63 255)
  , (2048, makeColorI 237 194 46 255)
  ]


getColor :: Int -> Color
getColor x = maybe white id (lookup x colorZipper)


quarterRoundedRect :: Int -> Float -> Float -> Float -> Path
quarterRoundedRect n w h r =
  [(0, 0), (0, h / 2)]
    ++ (reverse $ arcPath n (w / 2 - r, h / 2 - r) r)
    ++ [(w / 2, 0)]


drawQuarterRoundedRect :: Int -> Float -> Float -> Float -> Picture
drawQuarterRoundedRect n w h r = polygon $ quarterRoundedRect n w h r


outlineQuarterRoundedRect :: Int -> Float -> Float -> Float -> Picture
outlineQuarterRoundedRect n w h r = line $ quarterRoundedRect n w h r


-- takes width and height and radius and makes a filled rounded rectangle
-- the int is the precision / number of points
roundedRect :: Int -> Float -> Float -> Float -> Picture
roundedRect n w h r =
  pictures
    [ drawQuarterRoundedRect n w h r
    , rotate 90 $ drawQuarterRoundedRect n w h r
    , rotate 180 $ drawQuarterRoundedRect n w h r
    , rotate 270 $ drawQuarterRoundedRect n w h r
    ]


-- takes x, y, r, and theta and returns (x+r*cos theta, y+r*sin theta)
getPoint :: Float -> Float -> Float -> Float -> (Float, Float)
getPoint x y r th = (x + r * cos th, y + r * sin th)


-- takes center and radius and returns 90-degree arc path with n points
arcPath :: Int -> (Float, Float) -> Float -> Path
arcPath n (x, y) r =
  map (getPoint x y r) $
    [0.0] ++ (map (\x -> pi / 2 / (fromIntegral x)) $ reverse [1 .. n + 1])


tileRoundness = 4
tilePrecision = 10


-- Takes x-offset and draws the tile background
-- maybe unroll this into drawTile?
tileBackColor = makeColorI 205 192 180 255
drawTileBack :: Float -> Picture
drawTileBack x =
  color
    tileBackColor
    (translate x 0 (roundedRect tilePrecision tileS tileS tileRoundness))


-- Takes x-offset and tile and draws the tile itself
drawTile :: Float -> Tile -> Picture
drawTile x tile =
  let background =
        [ color (getColor $ val tile) $
            roundedRect tilePrecision tileS tileS tileRoundness
        ]
      number =
        if val tile > 0
          then
            [ translate (-20) (-10) $
                scale textScale textScale $
                  text $
                    T.pack $
                      show $
                        val tile
            ]
          else []
      curScale =
        if (popInTime tile) > 0
          then (1 - (popInTime tile))
          else (1 + (popOutTime tile))
  in  pictures
        [ drawTileBack x
        , translate x 0 $ scale curScale curScale $ pictures $ background ++ number
        ]


drawRow :: Row -> Picture
drawRow [i, j, k, l] =
  translate
    (-300)
    0
    ( pictures
        [ drawTile 0 i
        , drawTile rowHgt j
        , drawTile (rowHgt * 2) k
        , drawTile (rowHgt * 3) l
        ]
    )


--------------------------------------------
-- Board handling (moving and stuff)      --
--------------------------------------------

toWorldFunc :: ([[Tile]] -> [[Tile]]) -> World -> World
toWorldFunc tf w = w{board = tf (board w)}


scootLambda :: Tile -> [Tile] -> [Tile]
scootLambda y [] = [y]
scootLambda y [x] = if val x == 0 then [x, y] else [y, x] -- TODO add animations
scootLambda y (x : xs) = if val x == 0 then x : y : xs else y : x : xs


-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2] scootRowRightOnce :: [Int] -> [Int]
scootRowRightOnce = foldr scootLambda []


-- does scootRight three times
scootRowRight :: [Tile] -> [Tile]
scootRowRight = scootRowRightOnce . scootRowRightOnce . scootRowRightOnce


-- scoots whole board
scootRight :: [[Tile]] -> [[Tile]]
scootRight = map scootRowRight


scoot :: Maybe Direction -> [[Tile]] -> [[Tile]]
scoot Nothing = id
scoot (Just R) = scootRight
scoot (Just U) = reverse . transpose . scootRight . transpose . reverse
scoot (Just L) =
  transpose . reverse . transpose . scootRight . transpose . reverse . transpose
scoot (Just D) = transpose . scootRight . transpose


--------------------------------------------
-- Comboing (TODO: REFACTOR)              --
--------------------------------------------

comboLambda :: Tile -> ([Tile], Int) -> ([Tile], Int)
comboLambda y ([], s) = ([y], s)
comboLambda y ((x : xs), s) =
  if val x == val y && val x > 0
    then
      ( (makeTile 0) : (Tile{val = val x + val y, popOutTime = 0.1, popInTime = 0}) : xs
      , s + val x + val y
      )
    else (y : x : xs, s)


-- Takes a row and does combos to the right on all numbers *once*
-- Example: [2,2,0,0] -> [0,4,0,0] and [2,2,2,2] -> [0,4,0,4]
comboRowRight :: [Tile] -> ([Tile], Int)
comboRowRight = foldr comboLambda ([], 0)


-- does combos on whole board
comboRight :: World -> World
-- map (comboRowRight w) (board w) => [([Tile],Int)]
comboRight w =
  let (newBoard, scores) = unzip $ map comboRowRight (board w)
  in  w{board = newBoard, score = sum scores + score w}


combo :: Maybe Direction -> World -> World
combo Nothing = id
combo (Just R) = comboRight
combo (Just U) =
  (toWorldFunc $ reverse . transpose)
    . comboRight
    . (toWorldFunc $ transpose . reverse)
combo (Just L) =
  (toWorldFunc $ transpose . reverse . transpose)
    . comboRight
    . (toWorldFunc $ transpose . reverse . transpose)
combo (Just D) = (toWorldFunc transpose) . comboRight . (toWorldFunc transpose)


go :: Maybe Direction -> World -> World
go dir = (toWorldFunc $ scoot dir) . (combo dir) . (toWorldFunc $ scoot dir)
