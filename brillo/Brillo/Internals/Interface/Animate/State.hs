{-# OPTIONS_HADDOCK hide #-}

module Brillo.Internals.Interface.Animate.State (
  State (..),
  stateInit,
)
where


-- | Animation State
data State
  = State
  { stateAnimate :: !Bool
  -- ^ Whether the animation is running.
  , stateAnimateCount :: !Integer
  -- ^ How many times we've entered the animation loop.
  , stateAnimateStart :: !Bool
  -- ^ Whether this is the first frame of the animation.
  , stateAnimateTime :: !Double
  -- ^ Number of msec the animation has been running for
  , stateDisplayTime :: !Double
  -- ^ The time when we entered the display callback for the current frame.
  , stateDisplayTimeLast :: !Double
  , stateDisplayTimeClamp :: !Double
  -- ^ Clamp the minimum time between frames to this value (in seconds)
  --      Setting this to < 10ms probably isn't worthwhile.
  , stateGateTimeStart :: !Double
  -- ^ The time when the last call to the users render function finished.
  , stateGateTimeEnd :: !Double
  -- ^ The time when displayInWindow last finished (after sleeping to clamp fps).
  , stateGateTimeElapsed :: !Double
  -- ^ How long it took to draw this frame
  }


stateInit :: State
stateInit =
  State
    { stateAnimate = True
    , stateAnimateCount = 0
    , stateAnimateStart = True
    , stateAnimateTime = 0
    , stateDisplayTime = 0
    , stateDisplayTimeLast = 0
    , stateDisplayTimeClamp = 0.01
    , stateGateTimeStart = 0
    , stateGateTimeEnd = 0
    , stateGateTimeElapsed = 0
    }
