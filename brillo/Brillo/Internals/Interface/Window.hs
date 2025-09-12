{-# OPTIONS_HADDOCK hide #-}

-- |    The main display function.
module Brillo.Internals.Interface.Window (createWindow)
where

import Brillo.Data.Color
import Brillo.Internals.Color
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Debug
import Control.Monad
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL


-- | Open a window and use the supplied callbacks to handle window events.
createWindow ::
  (Backend a) =>
  a ->
  Display ->
  -- | Color to use when clearing.
  Color ->
  -- | Callbacks to use.
  [Callback] ->
  -- | Give the backend back to the caller before
  --   entering the main loop.
  (IORef a -> IO ()) ->
  IO ()
createWindow
  backend
  display
  clearColor
  callbacks
  eatBackend =
    do
      -- Turn this on to spew debugging info to stdout
      let debug = False

      -- Initialize backend state
      backendStateRef <- newIORef backend

      when debug $
        do putStr $ "* displayInWindow\n"

      -- Intialize backend
      initializeBackend backendStateRef debug

      -- Here we go!
      when debug $
        do putStr $ "* c window\n\n"

      -- Open window
      openWindow backendStateRef display

      -- Setup callbacks
      installDisplayCallback backendStateRef callbacks
      installWindowCloseCallback backendStateRef
      installReshapeCallback backendStateRef callbacks
      installKeyMouseCallback backendStateRef callbacks
      installMotionCallback backendStateRef callbacks
      installDropCallback backendStateRef callbacks
      installIdleCallback backendStateRef callbacks

      -- we don't need the depth buffer for 2d.
      GL.depthFunc $= Just GL.Always

      -- always clear the buffer to white
      GL.clearColor $= glColor4OfColor clearColor

      -- Dump some debugging info
      when debug $
        do
          dumpBackendState backendStateRef
          dumpFramebufferState
          dumpFragmentState

      eatBackend backendStateRef

      when debug $
        do putStr "* entering mainloop..\n"

      -- Start the main backend loop
      runMainLoop backendStateRef

      when debug $
        putStr "* all done\n"
