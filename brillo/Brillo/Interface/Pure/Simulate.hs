{-# LANGUAGE RankNTypes #-}

-- We export this stuff separately so we don't clutter up the
-- API of the Brillo module.

{-| Simulate mode is for producing an animation of some model who's picture
  changes over finite time steps. The behavior of the model can also depent
  on the current `ViewPort`.
-}
module Brillo.Interface.Pure.Simulate (
  module Brillo.Data.Display,
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  simulate,
  ViewPort (..),
)
where

import Brillo.Data.Color
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Data.ViewPort
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Simulate


{-| Run a finite-time-step simulation in a window. You decide how the model is represented,
     how to convert the model to a picture, and how to advance the model for each unit of time.
     This function does the rest.

  Once the window is open you can use the same commands as with `display`.
-}
simulate ::
  -- | Display mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of simulation steps to take for each second of real time.
  Int ->
  -- | The initial model.
  model ->
  -- | A function to convert the model to a picture.
  (model -> Picture) ->
  -- | A function to step the model one iteration. It is passed the
  --     current viewport and the amount of time for this simulation
  --     step (in seconds).
  (ViewPort -> Float -> model -> model) ->
  IO ()
simulate
  display
  backColor
  simResolution
  modelStart
  modelToPicture
  modelStep =
    do
      _ <-
        simulateWithBackendIO
          defaultBackendState
          display
          backColor
          simResolution
          modelStart
          (return . modelToPicture)
          (\view time model -> return $ modelStep view time model)
      return ()
