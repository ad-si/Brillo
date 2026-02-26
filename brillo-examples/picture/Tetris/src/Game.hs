{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Game (runGame) where

import Config
import Drawing
import World

import Brillo
import Brillo.Interface.Pure.Game
import Control.Monad (foldM)
import Control.Monad.Reader
import Control.Monad.State
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


runGame :: IO ()
runGame = do
  fp <- resolveFont
  let cfg = defaultAppConfig fp
  game <- initState cfg
  play
    (window cfg) -- Display mode.
    background -- Background color.
    fps -- Number of simulation steps to take for each second of real time.
    game -- The initial world.
    (render cfg) -- A function to convert the world a picture.
    (handler cfg) -- A function to handle input events.
    update -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


window :: AppConfig -> Display
window cfg = InWindow "Tetris" (windowSize cfg) (windowPosition cfg)


-- | Smallest common multiple
fps :: Int
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
    movingDown game_ =
      if (pressedKeyDown game_ && (not $ isPause game_))
        then (shiftDownFigure game_)
        else game_


-- | A function to handle input events.
handler :: AppConfig -> Event -> TetrisGame -> TetrisGame


-- | Handles "pause" button
handler _cfg (EventKey (Char 'p') Down _ _) game =
  game{isPause = not $ isPause game}
-- \| Handles "reset" button
handler _cfg (EventKey (Char 'r') Down _ _) game =
  resetGame game
-- \| Handles Enter to restart on game over
handler _cfg (EventKey (SpecialKey KeyEnter) Down _ _) game
  | gameOver game = resetGame game
  | otherwise = game
-- \| Handles click on restart button
handler cfg (EventKey (MouseButton LeftButton) Down _ (mx, my)) game
  | gameOver game =
      let (bx, by, bw, bh) = restartButtonBounds cfg
      in  if mx >= bx && mx <= bx + bw && my >= by && my <= by + bh
            then resetGame game
            else game
  | otherwise = game
-- \| Handles "left" button press
handler _cfg (EventKey (SpecialKey KeyLeft) Down _ _) game =
  case isPause game of
    False -> shiftLeftFigure game
    True -> game
-- \| Handles "right" button press
handler _cfg (EventKey (SpecialKey KeyRight) Down _ _) game =
  case isPause game of
    False -> shiftRightFigure game
    True -> game
-- \| Handles "down" button press
handler _cfg (EventKey (SpecialKey KeyDown) Down _ _) game =
  game{pressedDown = True}
-- \| Handles "down" button up
handler _cfg (EventKey (SpecialKey KeyDown) Up _ _) game =
  game{pressedDown = False}
-- \| Handles "up" button
handler _cfg (EventKey (SpecialKey KeyUp) Down _ _) game =
  case isPause game of
    False -> rotateFigure game
    True -> game
-- \| Handles "space" button - hard drop
handler _cfg (EventKey (SpecialKey KeySpace) Down _ _) game =
  case isPause game of
    False -> dropFigure game
    True -> game
-- \| Handles "esc" button
handler _cfg (EventKey (SpecialKey KeyEsc) Down _ _) _ =
  error "close game" -- wow!

-- \| Handles the rest input
handler _ _ game = game


-- Font resolution -----------------------------------------------------------

candidateFonts :: [FilePath]
candidateFonts =
  [ "/System/Library/Fonts/Supplemental/Arial.ttf"
  , "/Library/Fonts/Arial.ttf"
  , "/System/Library/Fonts/Supplemental/Helvetica.ttc"
  , "/System/Library/Fonts/Supplemental/Tahoma.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  , "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
  , "C:\\Windows\\Fonts\\arial.ttf"
  , "C:\\Windows\\Fonts\\segoeui.ttf"
  ]


resolveFont :: IO FilePath
resolveFont = do
  found <- foldM pick Nothing candidateFonts
  case found of
    Just fp -> pure fp
    Nothing -> do
      hPutStrLn stderr $
        unlines
          [ "brillo-tetris: unable to locate a TrueType font."
          , "Tried the following paths:"
          , unlines (map ("  - " <>) candidateFonts)
          ]
      exitFailure
  where
    pick (Just fp) _ = pure (Just fp)
    pick Nothing candidate = do
      exists <- doesFileExist candidate
      pure $ if exists then Just candidate else Nothing
