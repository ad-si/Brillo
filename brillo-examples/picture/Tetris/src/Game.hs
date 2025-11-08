{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Game (runGame) where

import Config
import Drawing
import Figures
import World

import Brillo
import Brillo.Interface.Pure.Game
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe


runGame :: IO ()
runGame = do
  let cfg = defaultAppConfig
  game <- initState cfg
  play
    (window cfg) -- Display mode.
    background -- Background color.
    fps -- Number of simulation steps to take for each second of real time.
    game -- The initial world.
    (render cfg) -- A function to convert the world a picture.
    handler -- A function to handle input events.
    update -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


window :: AppConfig -> Display
window cfg = InWindow "Tetris" (windowSize cfg) (windowPosition cfg)


-- | Smallest common multiple
fps = 24


-- | Hardness fraps per second * (1/fps)
hardnessMod :: Hardness -> Integer
hardnessMod Noob = 24
hardnessMod Beginner = 12
hardnessMod Average = 8
hardnessMod Skilled = 6
hardnessMod Masterful = 4
hardnessMod Insane = 3
hardnessMod Godlike = 2


background :: Color
background = (makeColorI 0 0 0 0)


initState :: AppConfig -> IO TetrisGame
initState = runReaderT (initialState)


-- | Render function for game
render :: AppConfig -> TetrisGame -> Picture
render cfg game = runReader (evalStateT drawWindow game) cfg


-- | Updates game state by shifting current falling figure down
update :: Float -> TetrisGame -> TetrisGame
update _ game = movingDown $ case (isPause game, gameOver game) of
  (False, False) -> case (needUpdate) of
    True -> shiftDownFigure (game{frapsCounter = 0})
    False -> game{frapsCounter = (frapsCounter game) + 1}
  (_, True) -> game{isPause = True}
  (True, _) -> game
  where
    needUpdate = (== 0) $ mod ((+ 1) $ frapsCounter game) (hardnessMod $ hardness game)
    movingDown game =
      if (pressedKeyDown game && (not $ isPause game))
        then (shiftDownFigure game)
        else game


-- | A function to handle input events.
handler :: Event -> TetrisGame -> TetrisGame


-- | Handles "pause" button
handler (EventKey (Char 'p') Down _ _) game =
  game{isPause = not $ isPause game}
-- \| Handles "reset" button
handler (EventKey (Char 'r') Down _ _) game =
  resetGame game
-- \| Handles "left" button press
handler (EventKey (SpecialKey KeyLeft) Down _ _) game =
  case isPause game of
    False -> shiftLeftFigure game
    True -> game
-- \| Handles "right" button press
handler (EventKey (SpecialKey KeyRight) Down _ _) game =
  case isPause game of
    False -> shiftRightFigure game
    True -> game
-- \| Handles "down" button press
handler (EventKey (SpecialKey KeyDown) Down _ _) game =
  game{pressedDown = True}
-- \| Handles "down" button up
handler (EventKey (SpecialKey KeyDown) Up _ _) game =
  game{pressedDown = False}
-- \| Handles "up" button
handler (EventKey (SpecialKey KeyUp) Down _ _) game =
  case isPause game of
    False -> rotateFigure game
    True -> game
-- \| Handles "space" button - hard drop
handler (EventKey (SpecialKey KeySpace) Down _ _) game =
  case isPause game of
    False -> dropFigure game
    True -> game
-- \| Handles "esc" button
handler (EventKey (SpecialKey KeyEsc) Down _ _) game =
  error "close game" -- wow!

-- \| Handles the rest input
handler _ game = game
