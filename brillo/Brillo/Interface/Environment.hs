module Brillo.Interface.Environment where

import Data.IORef (newIORef)
import Data.Text qualified as T

import Brillo.Data.FileDialog (FileDialog)
import Brillo.Internals.Interface.Backend (defaultBackendState)
import Brillo.Internals.Interface.Backend.Types qualified as Backend.Types
import Brillo.Internals.TinyFileDialogs qualified as TinyFileDialogs


{-| Get the size of the screen, in pixels.

  This will be the size of the rendered brillo image when
  fullscreen mode is enabled.
-}
getScreenSize :: IO (Int, Int)
getScreenSize = do
  backendStateRef <- newIORef defaultBackendState
  Backend.Types.initializeBackend backendStateRef False
  Backend.Types.getScreenSize backendStateRef


{-| Open a file dialog to select files/directories.

  Returns a list of paths, or `Nothing` if the dialog was cancelled.
-}
openFileDialog :: FileDialog -> IO (Maybe [FilePath])
openFileDialog fileDialog = do
  backendStateRef <- newIORef defaultBackendState
  Backend.Types.initializeBackend backendStateRef False
  Backend.Types.openFileDialog backendStateRef fileDialog


{-| Open a save file dialog.

  Returns the selected file path, or `Nothing` if the dialog was cancelled.
-}
saveFileDialog ::
  -- | Dialog title
  T.Text ->
  -- | Default file path
  T.Text ->
  -- | Filter patterns like @[\"*.svg\", \"*.png\"]@
  [T.Text] ->
  -- | Filter description like @\"Image files\"@
  T.Text ->
  IO (Maybe FilePath)
saveFileDialog title defaultPath filterPatterns filterDescription = do
  result <-
    TinyFileDialogs.saveFileDialog
      title
      defaultPath
      filterPatterns
      filterDescription
  pure $ fmap T.unpack result
