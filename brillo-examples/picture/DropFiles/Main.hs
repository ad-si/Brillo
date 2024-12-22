-- | Drag & drop files onto the window to load them
module Main where

import Brillo
import Brillo.Interface.Pure.Game
import Data.Function ((&))
import Data.Functor ((<&>))


width :: (Num a) => a
width = 600


height :: (Num a) => a
height = 600


data State
  = -- | Dropped files and directories
    State [FilePath]


-- | Convert the state to a picture
makePicture :: State -> Picture
makePicture (State filePaths) =
  Pictures $
    filePaths & zip [(1 :: Int) ..] <&> \(i, filePath) ->
      Translate (-width / 2) (fromIntegral (-25 * i) + (height / 2)) $
        Scale 0.1 0.1 $
          Text filePath


-- | Handle drag & drop events
handleEvent :: Event -> State -> State
handleEvent event state =
  case event of
    EventDrop filePaths -> State filePaths
    _ -> state


stepWorld :: Float -> State -> State
stepWorld _ = id


main :: IO ()
main = do
  let state = State []
  play
    (InWindow "DropFiles" (width, height) (0, 0))
    white
    100
    state
    makePicture
    handleEvent
    stepWorld
