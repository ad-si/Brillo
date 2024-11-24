module Brillo.Interface.Environment where

import Data.IORef (newIORef)

import Brillo.Internals.Interface.Backend (defaultBackendState)
import Brillo.Internals.Interface.Backend.Types qualified as Backend.Types


{-| Get the size of the screen, in pixels.

  This will be the size of the rendered brillo image when
  fullscreen mode is enabled.
-}
getScreenSize :: IO (Int, Int)
getScreenSize = do
  backendStateRef <- newIORef defaultBackendState
  Backend.Types.initializeBackend backendStateRef False
  Backend.Types.getScreenSize backendStateRef
