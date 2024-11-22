{-# LANGUAGE RankNTypes #-}

-- We export this stuff separately so we don't clutter up the
-- API of the Brillo module.

-- | Simulate mode is for producing an animation of some model who's picture
--   changes over finite time steps. The behavior of the model can also depent
--   on the current `ViewPort`.
module Brillo.Interface.IO.Simulate
        ( module Brillo.Data.Display
        , module Brillo.Data.Picture
        , module Brillo.Data.Color
        , simulateIO
        , ViewPort(..))
where
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Data.Color
import Brillo.Data.ViewPort
import Brillo.Internals.Interface.Simulate
import Brillo.Internals.Interface.Backend


simulateIO :: forall model
        .  Display               -- ^ Display mode.
        -> Color                 -- ^ Background color.
        -> Int                   -- ^ Number of simulation steps to take for each second of real time.
        -> model                 -- ^ The initial model.
        -> (model -> IO Picture) -- ^ A function to convert the model to a picture.
        -> (ViewPort -> Float -> model -> IO model)
                                 -- ^ A function to step the model one iteration. It is passed the
                                 --     current viewport and the amount of time for this simulation
                                 --     step (in seconds).
        -> IO ()

simulateIO = simulateWithBackendIO defaultBackendState
