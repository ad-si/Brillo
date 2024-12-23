{-# OPTIONS_HADDOCK hide #-}

-- | Re-export event callbacks.
module Brillo.Internals.Interface.Callback (
  Callback (..),
  DisplayCallback,
  KeyboardMouseCallback,
  MotionCallback,
  DropCallback,
  IdleCallback,
  ReshapeCallback,
)
where

import Brillo.Internals.Interface.Backend.Types
