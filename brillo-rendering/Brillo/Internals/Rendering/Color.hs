{-# OPTIONS_HADDOCK hide #-}

module Brillo.Internals.Rendering.Color where

import Brillo.Internals.Data.Color (Color (..))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Unsafe.Coerce (unsafeCoerce)


-- | Convert one of our Colors to OpenGL's representation.
glColor4OfColor :: Color -> GL.Color4 a
glColor4OfColor color =
  case color of
    RGBA r g b a ->
      let rF = unsafeCoerce r
          gF = unsafeCoerce g
          bF = unsafeCoerce b
          aF = unsafeCoerce a
      in  GL.Color4 rF gF bF aF
{-# INLINE glColor4OfColor #-}
