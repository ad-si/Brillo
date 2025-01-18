{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo.Data.Color (white)
import Brillo.Data.Display (Display (InWindow))
import Brillo.Data.FileDialog (
  FileDialog (..),
  SelectionMode (..),
 )
import Brillo.Data.Picture (Picture (Pictures, Scale, ThickText, Translate))
import Brillo.Interface.Environment (openFileDialog)
import Brillo.Interface.IO.Game (
  Event (EventKey),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton, RightButton),
  playIO,
 )
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T


size :: (Num width, Num height) => (width, height)
size = (600, 600)


data State
  = NotAsked
  | Success [FilePath]
  | Failure Text


moveToTopLeftWithOffset :: Float -> Picture -> Picture
moveToTopLeftWithOffset offset = do
  let
    w = fst (size :: (Float, Float))
    h = snd (size :: (Float, Float))
  Translate (-(w / 2) + offset) (h / 2 - offset)


makePicture :: State -> IO Picture
makePicture state =
  case state of
    NotAsked ->
      pure $
        Pictures
          [ moveToTopLeftWithOffset 50 $
              Scale 0.2 0.2 $
                ThickText "Left-click anywhere to pick files or" 3
          , moveToTopLeftWithOffset 100 $
              Scale 0.2 0.2 $
                ThickText "right-click to pick a directory" 3
          ]
    Failure errorMessage ->
      pure $
        Pictures
          [ moveToTopLeftWithOffset 30 $
              Scale 0.1 0.1 $
                ThickText errorMessage 2
          ]
    Success filePaths ->
      pure $
        Pictures $
          filePaths & zip [(1 :: Int) ..] <&> \(i, filePath) ->
            moveToTopLeftWithOffset 0 $
              Translate 10 (fromIntegral (-(25 * i))) $
                Scale 0.1 0.1 $
                  ThickText (T.pack filePath) 2


handleEvent :: Event -> State -> IO State
handleEvent event state =
  case event of
    EventKey (MouseButton mouseButton) Down _modifiers _point -> do
      filePathsMb <-
        openFileDialog $
          FileDialog
            { title = "Pick Files"
            , defaultPath = "."
            , filterPatterns = []
            , filterDescription = "All files"
            , selectionMode = case mouseButton of
                LeftButton -> MultiFileSelect
                RightButton -> SingleDirectorySelect
                _ -> MultiFileSelect
            }
      case filePathsMb of
        Just filePaths -> return $ Success filePaths
        Nothing -> return $ Failure "No files were picked"
    _ -> return state


stepWorld :: Float -> State -> IO State
stepWorld _ = return


main :: IO ()
main = do
  let state = NotAsked
  playIO
    (InWindow "PickFiles" size (0, 0))
    white
    100
    state
    makePicture
    handleEvent
    stepWorld
