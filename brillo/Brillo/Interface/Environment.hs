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


{-| Get the scale factor between window coordinates and logical UI units,
  e.g. @1.5@ for a display configured to 150 % scaling.

  Multiply window sizes and drawing coordinates with it
  to keep an interface at a constant physical size across displays.
  It is always 1 on platforms whose window coordinates
  already are logical units (e.g. macOS).
-}
getDpiScale :: IO Float
getDpiScale = do
  backendStateRef <- newIORef defaultBackendState
  Backend.Types.initializeBackend backendStateRef False
  Backend.Types.getDpiScale backendStateRef


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
