{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use unless" #-}

module Brillo.Internals.Rendering.Picture (renderPicture)
where

import Control.Monad (forM_, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (find)
import Foreign.ForeignPtr (withForeignPtr)
import GHC.IO.StdHandles (stderr)
import Graphics.Rendering.OpenGL (get, ($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Graphics.Rendering.OpenGL.GLU.Errors qualified as GLU
import System.IO (hPutStrLn)
import System.Mem.StableName (makeStableName)

import Brillo.Internals.Data.Color (Color (RGBA))
import Brillo.Internals.Data.Picture (
  BitmapFormat (pixelFormat, rowOrder),
  Picture (..),
  PixelFormat (PxABGR, PxRGBA),
  Rectangle (Rectangle, rectPos, rectSize),
  RowOrder (BottomToTop, TopToBottom),
  rectAtOrigin,
 )
import Brillo.Internals.Rendering.Bitmap (BitmapData (..), bitmapPath)
import Brillo.Internals.Rendering.Circle (renderArc, renderCircle)
import Brillo.Internals.Rendering.Common (gf, gsizei)
import Brillo.Internals.Rendering.State (
  State (
    stateBlendAlpha,
    stateColor,
    stateLineSmooth,
    stateTextures,
    stateWireframe
  ),
  Texture (..),
 )
import Brillo.Internals.Rendering.TrueTypeFont (renderTrueTypeText)
import Brillo.Internals.Rendering.VectorFont as VF (canvastextFont, renderSafe)


{-| Render a picture into the current OpenGL context.

  Assumes that the OpenGL matrix mode is set to @Modelview@
-}
renderPicture ::
  -- | Current rendering state.
  State ->
  -- | View port scale, which controls the level of detail.
  --   Use 1.0 to start with.
  Float ->
  -- | Picture to render.
  Picture ->
  IO ()
renderPicture state circScale picture =
  do
    -- Setup render state for world
    setLineSmooth state.stateLineSmooth
    setBlendAlpha state.stateBlendAlpha

    -- Draw the picture
    checkErrors "before drawPicture."
    drawPicture state circScale picture
    checkErrors "after drawPicture."


drawPicture :: State -> Float -> Picture -> IO ()
drawPicture state circScale picture =
  {-# SCC "drawComponent" #-}
  case picture of
    --
    Blank ->
      return ()
    --
    Line path ->
      GL.renderPrimitive GL.LineStrip $
        vertexPFs path
    --
    LineSmooth path -> do
      GL.lineSmooth $= GL.Enabled
      GL.renderPrimitive GL.LineStrip $
        vertexPFs path
      GL.lineSmooth $= GL.Disabled
    --
    ThickLine path thickness -> do
      widthRange <- GL.aliasedLineWidthRange
      when (thickness < fst widthRange || thickness > snd widthRange) $
        hPutStrLn stderr $
          "Error: The line width "
            <> show thickness
            <> " is outside the supported range of "
            <> show widthRange

      oldLineWidth <- get GL.lineWidth
      GL.lineWidth $= gf thickness
      GL.renderPrimitive GL.LineStrip $ vertexPFs path
      GL.lineWidth $= oldLineWidth
    --
    ThickLineSmooth path thickness -> do
      widthRange <- GL.smoothLineWidthRange
      when (thickness < fst widthRange || thickness > snd widthRange) $
        hPutStrLn stderr $
          "Error: The line width "
            <> show thickness
            <> " is outside the supported range of "
            <> show widthRange

      oldLineWidth <- get GL.lineWidth
      GL.lineWidth $= gf thickness
      GL.lineSmooth $= GL.Enabled
      GL.renderPrimitive GL.LineStrip $ vertexPFs path
      GL.lineSmooth $= GL.Disabled
      GL.lineWidth $= oldLineWidth
    --
    Polygon path
      | state.stateWireframe ->
          GL.renderPrimitive GL.LineLoop $
            vertexPFs path
      | otherwise ->
          GL.renderPrimitive GL.Polygon $
            vertexPFs path
    -- circle
    Circle radius ->
      renderCircle 0 0 circScale radius 0
    ThickCircle radius thickness ->
      renderCircle 0 0 circScale radius thickness
    -- arc
    Arc a1 a2 radius ->
      renderArc 0 0 circScale radius a1 a2 0
    ThickArc a1 a2 radius thickness ->
      renderArc 0 0 circScale radius a1 a2 thickness
    -- Vector font text
    Text str -> do
      let
        characters :: [[(Double, Double)]]
        characters = renderSafe canvastextFont str

      oldLineWidth <- get GL.lineWidth
      GL.lineWidth $= gf 3.0
      GL.preservingMatrix $ do
        GL.scale (gf 5) (gf 5) 0
        forM_ characters $ \stroke -> do
          GL.renderPrimitive GL.LineStrip $ do
            forM_ stroke $ \(x, y) -> do
              GL.vertex $ GL.Vertex2 x y
      GL.lineWidth $= oldLineWidth
    --
    ThickText str thickness -> do
      let
        characters :: [[(Double, Double)]]
        characters = renderSafe canvastextFont str

      oldLineWidth <- get GL.lineWidth
      GL.lineWidth $= gf thickness
      GL.preservingMatrix $ do
        GL.scale (gf 5) (gf 5) 0
        forM_ characters $ \stroke -> do
          GL.renderPrimitive GL.LineStrip $ do
            forM_ stroke $ \(x, y) -> do
              GL.vertex $ GL.Vertex2 x y
      GL.lineWidth $= oldLineWidth
    TrueTypeText fontPath pixelHeight str ->
      renderTrueTypeText fontPath pixelHeight str
    -- colors with float components.
    Color col p
      | state.stateColor -> do
          oldColor <- get GL.currentColor

          let RGBA r g b a = col

          GL.currentColor $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
          drawPicture state circScale p
          GL.currentColor $= oldColor
      | otherwise ->
          drawPicture state circScale p
    -- Translation --------------------------
    -- Easy translations are done directly to avoid calling GL.perserveMatrix.
    Translate posX posY (Circle radius) ->
      renderCircle posX posY circScale radius 0
    Translate posX posY (ThickCircle radius thickness) ->
      renderCircle posX posY circScale radius thickness
    Translate posX posY (Arc a1 a2 radius) ->
      renderArc posX posY circScale radius a1 a2 0
    Translate posX posY (ThickArc a1 a2 radius thickness) ->
      renderArc posX posY circScale radius a1 a2 thickness
    Translate tx ty (Rotate deg p) ->
      GL.preservingMatrix $
        do
          GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
          GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
          drawPicture state circScale p
    Translate tx ty p ->
      GL.preservingMatrix $
        do
          GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
          drawPicture state circScale p

    -- Rotation -----------------------------
    -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
    Rotate _ (Circle radius) ->
      renderCircle 0 0 circScale radius 0
    Rotate _ (ThickCircle radius thickness) ->
      renderCircle 0 0 circScale radius thickness
    Rotate deg (Arc a1 a2 radius) ->
      renderArc 0 0 circScale radius (a1 - deg) (a2 - deg) 0
    Rotate deg (ThickArc a1 a2 radius thickness) ->
      renderArc 0 0 circScale radius (a1 - deg) (a2 - deg) thickness
    Rotate deg p ->
      GL.preservingMatrix $
        do
          GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
          drawPicture state circScale p

    -- Scale --------------------------------
    Scale sx sy p ->
      GL.preservingMatrix $
        do
          GL.scale (gf sx) (gf sy) 1
          let mscale = max sx sy
          drawPicture state (circScale * mscale) p
    Bitmap imgData ->
      let (width, height) = bitmapSize imgData
      in  drawPicture state circScale $
            BitmapSection (rectAtOrigin width height) imgData
    BitmapSection
      Rectangle
        { rectPos = imgSectionPos
        , rectSize = imgSectionSize
        }
      imgData@BitmapData
        { bitmapSize = (width, height)
        , bitmapCacheMe = cacheMe
        } ->
        do
          let rowInfo =
                -- calculate texture coordinates
                -- remark:
                --   On some hardware, using exact "integer" coordinates causes texture coords
                --   with a component == 0  flip to -1. This appears as the texture flickering
                --   on the left and sometimes show one additional row of pixels outside the
                --   given rectangle
                --   To prevent this we add an "epsilon-border".
                --   This has been testet to fix the problem.
                let defTexCoords =
                      map
                        (\(x, y) -> (x / fromIntegral width, y / fromIntegral height))
                        [ vecMap (+ eps) (+ eps) $ toFloatVec imgSectionPos
                        , vecMap (subtract eps) (+ eps) $
                            toFloatVec
                              ( fst imgSectionPos + fst imgSectionSize
                              , snd imgSectionPos
                              )
                        , vecMap (subtract eps) (subtract eps) $
                            toFloatVec
                              ( fst imgSectionPos + fst imgSectionSize
                              , snd imgSectionPos + snd imgSectionSize
                              )
                        , vecMap (+ eps) (subtract eps) $
                            toFloatVec
                              ( fst imgSectionPos
                              , snd imgSectionPos + snd imgSectionSize
                              )
                        ] ::
                        [(Float, Float)]
                    toFloatVec = vecMap fromIntegral fromIntegral
                    vecMap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
                    vecMap f g (x, y) = (f x, g y)
                    eps = 0.001 :: Float
                in  case rowOrder (bitmapFormat imgData) of
                      BottomToTop -> defTexCoords
                      TopToBottom -> reverse defTexCoords

          -- Load the image data into a texture,
          -- or grab it from the cache if we've already done that before.
          tex <- loadTexture state.stateTextures imgData cacheMe

          -- Set up wrap and filtering mode
          GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
          GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
          GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

          -- Enable texturing
          GL.texture GL.Texture2D $= GL.Enabled
          GL.textureFunction $= GL.Combine

          -- Set current texture
          GL.textureBinding GL.Texture2D $= Just (texObject tex)

          -- Set to opaque
          oldColor <- get GL.currentColor
          GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

          -- Draw textured polygon
          GL.renderPrimitive GL.Polygon
            $ forM_
              ( bitmapPath
                  (fromIntegral $ fst imgSectionSize)
                  (fromIntegral $ snd imgSectionSize)
                  `zip` rowInfo
              )
            $ \((polygonCoordX, polygonCoordY), (textureCoordX, textureCoordY)) ->
              do
                GL.texCoord $ GL.TexCoord2 (gf textureCoordX) (gf textureCoordY)
                GL.vertex $ GL.Vertex2 (gf polygonCoordX) (gf polygonCoordY)

          -- Restore color
          GL.currentColor $= oldColor

          -- Disable texturing
          GL.texture GL.Texture2D $= GL.Disabled

          -- Free uncachable texture objects.
          freeTexture tex
    Pictures ps ->
      mapM_ (drawPicture state circScale) ps


-- Errors ---------------------------------------------------------------------
checkErrors :: String -> IO ()
checkErrors place =
  do
    errors <- get GLU.errors
    when (not $ null errors) $
      mapM_ (handleError place) errors


handleError :: String -> GLU.Error -> IO ()
handleError place err =
  case err of
    GLU.Error GLU.StackOverflow _ ->
      error $
        unlines
          [ "Brillo / OpenGL Stack Overflow " ++ show place
          , "  This program uses the Brillo vector graphics library, which tried to"
          , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
          , "  than your OpenGL implementation supports. The OpenGL spec requires"
          , "  all implementations to have a transform stack depth of at least 32,"
          , "  and Brillo tries not to push the stack when it doesn't have to, but"
          , "  that still wasn't enough."
          , ""
          , "  You should complain to your harware vendor that they don't provide"
          , "  a better way to handle this situation at the OpenGL API level."
          , ""
          , "  To make this program work you'll need to reduce the number of nested"
          , "  transforms used when defining the Picture given to Brillo. Sorry."
          ]
    _ ->
      error $
        unlines
          [ "Brillo / OpenGL Internal Error " ++ show place
          , "  Please report this at https://github.com/ad-si/Brillo/issues."
          , show err
          ]


-- Textures -------------------------------------------------------------------

{-| Load a texture into the OpenGL context, or retrieve the existing handle
  from our own cache.
-}
loadTexture ::
  -- | Existing texture cache.
  IORef [Texture] ->
  -- | Texture data.
  BitmapData ->
  -- | Force cache for newly loaded textures.
  Bool ->
  IO Texture
loadTexture refTextures imgData@BitmapData{bitmapSize = (width, height)} cacheMe =
  do
    textures <- readIORef refTextures

    -- Try and find this same texture in the cache.
    name <- makeStableName imgData
    let mTexCached =
          find
            ( \tex ->
                texName tex == name
                  && texWidth tex == width
                  && texHeight tex == height
            )
            textures

    case mTexCached of
      Just tex ->
        return tex
      Nothing ->
        do
          tex <- installTexture imgData
          when cacheMe $
            writeIORef refTextures (tex : textures)
          return tex


{-| Install a texture into the OpenGL context,
  returning the new texture handle.
-}
installTexture :: BitmapData -> IO Texture
installTexture bitmapData@(BitmapData _ fmt (width, height) cacheMe fptr) =
  do
    let glFormat =
          case pixelFormat fmt of
            PxABGR -> GL.ABGR
            PxRGBA -> GL.RGBA

    -- Allocate texture handle for texture
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just tex

    -- Sets the texture in imgData as the current texture
    -- This copies the data from the pointer into OpenGL texture memory,
    -- so it's ok if the foreignptr gets garbage collected after this.
    withForeignPtr fptr $
      \ptr ->
        GL.texImage2D
          GL.Texture2D
          GL.NoProxy
          0
          GL.RGBA8
          ( GL.TextureSize2D
              (gsizei width)
              (gsizei height)
          )
          0
          (GL.PixelData glFormat GL.UnsignedByte ptr)

    -- Make a stable name that we can use to identify this data again.
    -- If the user gives us the same texture data at the same size then we
    -- can avoid loading it into texture memory again.
    name <- makeStableName bitmapData

    return
      Texture
        { texName = name
        , texWidth = width
        , texHeight = height
        , texData = fptr
        , texObject = tex
        , texCacheMe = cacheMe
        }


{-| If this texture does not have its `cacheMe` flag set then delete it from
  OpenGL and free the GPU memory.
-}
freeTexture :: Texture -> IO ()
freeTexture tex
  | texCacheMe tex = return ()
  | otherwise = GL.deleteObjectNames [texObject tex]


-- Utils ----------------------------------------------------------------------

-- | Turn alpha blending on or off
setBlendAlpha :: Bool -> IO ()
setBlendAlpha state
  | state =
      do
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  | otherwise =
      do
        GL.blend $= GL.Disabled
        GL.blendFunc $= (GL.One, GL.Zero)


-- | Turn line smoothing on or off
setLineSmooth :: Bool -> IO ()
setLineSmooth state
  | state = GL.lineSmooth $= GL.Enabled
  | otherwise = GL.lineSmooth $= GL.Disabled


vertexPFs :: [(Float, Float)] -> IO ()
vertexPFs [] = return ()
vertexPFs ((x, y) : rest) =
  do
    GL.vertex $ GL.Vertex2 (gf x) (gf y)
    vertexPFs rest
{-# INLINE vertexPFs #-}
