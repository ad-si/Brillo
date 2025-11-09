module Main where

import Brillo
import Brillo.Interface.IO.Game
import Control.Concurrent
import Control.Monad (void)
import Data.Array
import Data.Maybe (isNothing)
import Data.Text qualified as T
import System.Random (randomRIO)


-- | Main entry point
main :: IO ()
main = do
  aiMove <- newEmptyMVar
  playIO
    (InWindow (T.pack "Tic Tac Toe") (600, 600) (10, 10))
    white
    30
    (initialBoard, X, aiMove, Playing)
    drawBoard
    (handleInput aiMove)
    (stepGame aiMove)


-- | Game state types
data Play = X | O deriving (Eq, Show)


type Board = Array (Int, Int) (Maybe Play)


data GameState = Playing | Won Play | Draw deriving (Eq, Show)


-- | Initial empty board
initialBoard :: Board
initialBoard = listArray ((0, 0), (2, 2)) (replicate 9 Nothing)


-- | Draw the game board
drawBoard :: (Board, Play, MVar Board, GameState) -> IO Picture
drawBoard (board, _, _, gameState) = return (grid <> plays <> status)
  where
    grid =
      pictures
        [ color (greyN 0.3) $ line [(-150, -50), (150, -50)]
        , color (greyN 0.3) $ line [(-150, 50), (150, 50)]
        , color (greyN 0.3) $ line [(-50, -150), (-50, 150)]
        , color (greyN 0.3) $ line [(50, -150), (50, 150)]
        ]

    plays = pictures [drawPlay x y p | ((x, y), p) <- assocs board]

    drawPlay _ _ Nothing = Blank
    drawPlay x y (Just p) =
      translate (fromIntegral (x - 1) * 100) (fromIntegral (1 - y) * 100) $
        drawPiece p

    drawPiece X =
      color red $
        pictures
          [ rotate 45 $ rectangleSolid 80 10
          , rotate (-45) $ rectangleSolid 80 10
          ]
    drawPiece O = color blue $ thickCircle 30 10

    status = case gameState of
      Won X ->
        pictures
          [ translate (-90) 220 $
              scale 0.3 0.3 $
                color red $
                  text (T.pack "X Wins!")
          , translate (-140) 180 $
              scale 0.15 0.15 $
                color (greyN 0.5) $
                  text (T.pack "Press R to restart")
          ]
      Won O ->
        pictures
          [ translate (-90) 220 $
              scale 0.3 0.3 $
                color blue $
                  text (T.pack "O Wins!")
          , translate (-140) 180 $
              scale 0.15 0.15 $
                color (greyN 0.5) $
                  text (T.pack "Press R to restart")
          ]
      Draw ->
        pictures
          [ translate (-60) 220 $
              scale 0.3 0.3 $
                color black $
                  text (T.pack "Draw!")
          , translate (-140) 180 $
              scale 0.15 0.15 $
                color (greyN 0.5) $
                  text (T.pack "Press R to restart")
          ]
      Playing -> Blank


-- | Handle user input
handleInput ::
  MVar Board ->
  Event ->
  (Board, Play, MVar Board, GameState) ->
  IO (Board, Play, MVar Board, GameState)
-- Restart game on 'R' key press
handleInput aiMove (EventKey (Char 'r') Down _ _) (_, _, mv, _) = do
  -- Clear any pending AI move
  _ <- tryTakeMVar aiMove
  return (initialBoard, X, mv, Playing)
-- Handle mouse click during play
handleInput
  aiMove
  (EventKey (MouseButton LeftButton) Up _ (x, y))
  (board, X, mv, Playing) = do
    let snapX = 1 + fromIntegral (floor ((x + 50) / 100) :: Integer) :: Int
        snapY = 1 - fromIntegral (floor ((y + 50) / 100) :: Integer) :: Int
        gridX = max 0 (min 2 snapX)
        gridY = max 0 (min 2 snapY)

    case board ! (gridX, gridY) of
      Just _ -> return (board, X, mv, Playing) -- Cell already occupied
      Nothing -> do
        let newBoard = board // [((gridX, gridY), Just X)]
            newState = checkGameState newBoard
        case newState of
          Playing -> do
            forkAi aiMove newBoard
            return (newBoard, O, mv, newState)
          _ -> return (newBoard, X, mv, newState)
handleInput _ _ state = return state


-- | Fork AI computation in background
forkAi :: MVar Board -> Board -> IO ()
forkAi aiMove board = void $ forkIO $ do
  -- Random delay to simulate thinking
  threadDelay =<< randomRIO (100000, 1000000)

  -- Find all valid moves
  let plays =
        [ board // [((x, y), Just O)]
        | x <- [0 .. 2]
        , y <- [0 .. 2]
        , isNothing (board ! (x, y))
        ]

  case plays of
    [] -> putMVar aiMove board -- No moves available
    _ -> do
      -- Pick a random move
      idx <- randomRIO (0, length plays - 1)
      putMVar aiMove (plays !! idx)


-- | Step the game forward
stepGame ::
  MVar Board ->
  Float ->
  (Board, Play, MVar Board, GameState) ->
  IO (Board, Play, MVar Board, GameState)
stepGame aiMove _ (board, O, mv, Playing) = do
  maybeBoard <- tryTakeMVar aiMove
  case maybeBoard of
    Nothing -> return (board, O, mv, Playing)
    Just newBoard ->
      let newState = checkGameState newBoard
      in  return (newBoard, X, mv, newState)
stepGame _ _ state = return state


-- | Check if the game is over
checkGameState :: Board -> GameState
checkGameState board
  | hasWon X board = Won X
  | hasWon O board = Won O
  | isFull board = Draw
  | otherwise = Playing


-- | Check if a player has won
hasWon :: Play -> Board -> Bool
hasWon player board =
  any (all (== Just player)) allLines
  where
    allLines =
      [[board ! (i, j) | i <- [0 .. 2]] | j <- [0 .. 2]] -- rows
        ++ [[board ! (i, j) | j <- [0 .. 2]] | i <- [0 .. 2]] -- columns
        ++ [[board ! (i, i) | i <- [0 .. 2]]] -- diagonal \
        ++ [[board ! (i, 2 - i) | i <- [0 .. 2]]] -- diagonal /


-- | Check if the board is full
isFull :: Board -> Bool
isFull board =
  all
    (\(_, cell) -> case cell of Just _ -> True; Nothing -> False)
    (assocs board)
