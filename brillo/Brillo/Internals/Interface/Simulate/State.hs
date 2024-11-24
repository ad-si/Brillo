{-# OPTIONS_HADDOCK hide #-}

module Brillo.Internals.Interface.Simulate.State (
  State (..),
  stateInit,
)
where


-- | Simulation state
data State
  = State
  { stateIteration :: !Integer
  -- ^ The iteration number we're up to.
  , stateResolution :: !Int
  -- ^ How many simulation setps to take for each second of real time
  , stateSimTime :: !Float
  -- ^ How many seconds worth of simulation we've done so far
  }


-- | Initial control state
stateInit :: Int -> State
stateInit resolution =
  State
    { stateIteration = 0
    , stateResolution = resolution
    , stateSimTime = 0
    }
