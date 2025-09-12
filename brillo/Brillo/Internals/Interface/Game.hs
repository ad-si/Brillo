{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Brillo.Internals.Interface.Game (
  playWithBackendIO,
  Event (..),
)
where

import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Data.ViewPort
import Brillo.Internals.Interface.Animate.State qualified as AN
import Brillo.Internals.Interface.Animate.Timing
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Callback qualified as Callback
import Brillo.Internals.Interface.Common.Exit
import Brillo.Internals.Interface.Event
import Brillo.Internals.Interface.Simulate.Idle
import Brillo.Internals.Interface.Simulate.State qualified as SM
import Brillo.Internals.Interface.ViewState.Reshape
import Brillo.Internals.Interface.Window
import Brillo.Rendering
import Data.IORef
import System.Mem


playWithBackendIO ::
  forall world a.
  (Backend a) =>
  -- | Initial state of the backend
  a ->
  -- | Display mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of simulation steps to take for each second of real time.
  Int ->
  -- | The initial world.
  world ->
  -- | A function to convert the world to a picture.
  (world -> IO Picture) ->
  -- | A function to handle input events.
  (Event -> world -> IO world) ->
  -- | A function to step the world one iteration.
  --   It is passed the period of time (in seconds) needing to be advanced.
  (Float -> world -> IO world) ->
  -- | Whether to use the callback_exit or not.
  Bool ->
  IO ()
playWithBackendIO
  backend
  display
  backgroundColor
  simResolution
  worldStart
  worldToPicture
  worldHandleEvent
  worldAdvance
  withCallbackExit =
    do
      let singleStepTime = 1

      -- make the simulation state
      stateSR <- newIORef $ SM.stateInit simResolution

      -- make a reference to the initial world
      worldSR <- newIORef worldStart

      -- make the initial GL view and render states
      viewSR <- newIORef viewPortInit
      animateSR <- newIORef AN.stateInit
      renderS_ <- initState
      renderSR <- newIORef renderS_

      let displayFun backendRef =
            do
              -- convert the world to a picture
              world <- readIORef worldSR
              picture <- worldToPicture world

              -- display the picture in the current view
              renderS <- readIORef renderSR
              viewPort <- readIORef viewSR

              windowSize <- getWindowDimensions backendRef

              -- render the frame
              displayPicture
                windowSize
                backgroundColor
                renderS
                (viewPortScale viewPort)
                (applyViewPortToPicture viewPort picture)

              -- perform GC every frame to try and avoid long pauses
              performGC

      let callbacks =
            [ Callback.Display (animateBegin animateSR)
            , Callback.Display displayFun
            , Callback.Display (animateEnd animateSR)
            , Callback.Idle
                ( callback_simulate_idle
                    stateSR
                    animateSR
                    (readIORef viewSR)
                    worldSR
                    (const worldAdvance)
                    singleStepTime
                )
            , callback_keyMouse worldSR viewSR worldHandleEvent
            , callback_motion worldSR worldHandleEvent
            , callback_drop worldSR worldHandleEvent
            , callback_reshape worldSR worldHandleEvent
            ]

      let exitCallback =
            if withCallbackExit then [callback_exit ()] else []

      createWindow
        backend
        display
        backgroundColor
        (callbacks ++ exitCallback)
        (\_ -> return ())


-- | Callback for KeyMouse events.
callback_keyMouse ::
  -- | ref to world state
  IORef world ->
  IORef ViewPort ->
  -- | fn to handle input events
  (Event -> world -> IO world) ->
  Callback
callback_keyMouse worldRef viewRef eventFn =
  KeyMouse (handle_keyMouse worldRef viewRef eventFn)


handle_keyMouse ::
  IORef a ->
  t ->
  (Event -> a -> IO a) ->
  KeyboardMouseCallback
handle_keyMouse worldRef _ eventFn backendRef key keyState keyMods pos =
  do
    ev <- keyMouseEvent backendRef key keyState keyMods pos
    world <- readIORef worldRef
    world' <- eventFn ev world
    writeIORef worldRef world'


-- | Callback for Motion events.
callback_motion ::
  -- | ref to world state
  IORef world ->
  -- | fn to handle input events
  (Event -> world -> IO world) ->
  Callback
callback_motion worldRef eventFn =
  Motion (handle_motion worldRef eventFn)


handle_motion ::
  IORef a ->
  (Event -> a -> IO a) ->
  MotionCallback
handle_motion worldRef eventFn backendRef pos =
  do
    ev <- motionEvent backendRef pos
    world <- readIORef worldRef
    world' <- eventFn ev world
    writeIORef worldRef world'


callback_drop ::
  IORef world ->
  (Event -> world -> IO world) ->
  Callback
callback_drop worldRef eventFn =
  Drop (handle_drop worldRef eventFn)


handle_drop ::
  IORef a ->
  (Event -> a -> IO a) ->
  DropCallback
handle_drop worldRef eventFn backendRef paths = do
  ev <- dropEvent backendRef paths
  world <- readIORef worldRef
  world' <- eventFn ev world
  writeIORef worldRef world'


-- | Callback for Handle reshape event.
callback_reshape ::
  IORef world ->
  (Event -> world -> IO world) ->
  Callback
callback_reshape worldRef eventFN =
  Reshape (handle_reshape worldRef eventFN)


handle_reshape ::
  IORef world ->
  (Event -> world -> IO world) ->
  ReshapeCallback
handle_reshape worldRef eventFn stateRef (width, height) =
  do
    world <- readIORef worldRef
    world' <- eventFn (EventResize (width, height)) world
    writeIORef worldRef world'
    viewState_reshape stateRef (width, height)
