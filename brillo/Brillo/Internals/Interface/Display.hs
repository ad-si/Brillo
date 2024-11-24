module Brillo.Internals.Interface.Display (displayWithBackend)
where

import Brillo.Data.Color
import Brillo.Data.Controller
import Brillo.Data.Picture
import Brillo.Data.ViewPort
import Brillo.Data.ViewState
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Callback qualified as Callback
import Brillo.Internals.Interface.Common.Exit
import Brillo.Internals.Interface.ViewState.KeyMouse
import Brillo.Internals.Interface.ViewState.Motion
import Brillo.Internals.Interface.ViewState.Reshape
import Brillo.Internals.Interface.Window
import Brillo.Rendering
import Data.IORef
import System.Mem


displayWithBackend
  :: (Backend a)
  => a
  -- ^ Initial state of the backend.
  -> Display
  -- ^ Display config.
  -> Color
  -- ^ Background color.
  -> IO Picture
  -- ^ Make the picture to draw.
  -> (Controller -> IO ())
  -- ^ Eat the controller
  -> IO ()
displayWithBackend
  backend
  displayMode
  background
  makePicture
  eatController =
    do
      viewSR <- newIORef viewStateInit
      renderS <- initState
      renderSR <- newIORef renderS

      let renderFun backendRef = do
            port <- viewStateViewPort <$> readIORef viewSR
            options <- readIORef renderSR
            windowSize <- getWindowDimensions backendRef
            picture <- makePicture

            displayPicture
              windowSize
              background
              options
              (viewPortScale port)
              (applyViewPortToPicture port picture)

            -- perform GC every frame to try and avoid long pauses
            performGC

      let callbacks =
            [ Callback.Display renderFun
            , -- Escape exits the program
              callback_exit ()
            , -- Viewport control with mouse
              callback_viewState_keyMouse viewSR
            , callback_viewState_motion viewSR
            , callback_viewState_reshape
            ]

      -- When we create the window we can pass a function to get a
      -- reference to the backend state. Using this we make a controller
      -- so the client can control the window asynchronously.
      createWindow backend displayMode background callbacks $
        \backendRef ->
          eatController $
            Controller
              { controllerSetRedraw =
                  do postRedisplay backendRef
              , controllerModifyViewPort =
                  \modViewPort ->
                    do
                      viewState <- readIORef viewSR
                      port' <- modViewPort $ viewStateViewPort viewState
                      let viewState' = viewState{viewStateViewPort = port'}
                      writeIORef viewSR viewState'
                      postRedisplay backendRef
              }
