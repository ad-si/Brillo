{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Brillo.Internals.Rendering.State (
  State (..),
  initState,
  Texture (..),
)
where

import Brillo.Internals.Rendering.Bitmap (BitmapData)
import Data.IORef (IORef, newIORef)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Graphics.Rendering.OpenGL.GL qualified as GL
import System.Mem.StableName (StableName)


{-| Abstract Brillo render state which holds references to textures
  loaded into the GPU context.
-}
data State
  = State
  { stateColor :: !Bool
  -- ^ Whether to use color
  , stateWireframe :: !Bool
  -- ^ Whether to force wireframe mode only
  , stateBlendAlpha :: !Bool
  -- ^ Whether to use alpha blending
  , stateLineSmooth :: !Bool
  -- ^ Whether to use line smoothing
  , stateTextures :: !(IORef [Texture])
  -- ^ Cache of Textures that we've sent to OpenGL.
  }


-- | A texture that we've sent to OpenGL.
data Texture
  = Texture
  { texName :: StableName BitmapData
  -- ^ Stable name derived from the `BitmapData` that the user gives us.
  , texWidth :: Int
  -- ^ Width of the image, in pixels.
  , texHeight :: Int
  -- ^ Height of the image, in pixels.
  , texData :: ForeignPtr Word8
  -- ^ Pointer to the Raw texture data.
  , texObject :: GL.TextureObject
  -- ^ The OpenGL texture object.
  , texCacheMe :: Bool
  -- ^ Whether we want to leave this in OpenGL texture memory between frames.
  }


{-| A mutable render state holds references to the textures currently loaded
  into the OpenGL context. To ensure that textures are cached in GPU memory,
  pass the same `State` each time you call `displayPicture` or `renderPicture`.
-}
initState :: IO State
initState =
  do
    textures <- newIORef []
    return
      State
        { stateColor = True
        , stateWireframe = False
        , stateBlendAlpha = True
        , stateLineSmooth = False
        , stateTextures = textures
        }
