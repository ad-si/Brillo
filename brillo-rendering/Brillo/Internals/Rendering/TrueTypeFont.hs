{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Rendering support for TrueType fonts using the FreeType bindings.
module Brillo.Internals.Rendering.TrueTypeFont (
  renderTrueTypeText,
) where

import Brillo.Internals.Rendering.Common (gf)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (try)
import Control.Monad (foldM, forM, forM_)
import Data.Either (partitionEithers)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Foreign (Ptr, nullPtr, peek)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import FreeType (
  FT_Bitmap (bBuffer, bRows, bWidth),
  FT_Face,
  FT_FaceRec (frGlyph),
  FT_GlyphSlotRec (
    gsrAdvance,
    gsrBitmap,
    gsrBitmap_left,
    gsrBitmap_top
  ),
  FT_Vector (vX),
  ft_Done_Face,
  ft_Done_FreeType,
  ft_Init_FreeType,
  ft_Load_Char,
  ft_New_Face,
  ft_Set_Pixel_Sizes,
  pattern FT_LOAD_RENDER,
 )
import FreeType.Exception (FtError)
import Graphics.Rendering.OpenGL (
  TextureFunction (Modulate),
  get,
  textureBinding,
  ($=),
 )
import Graphics.Rendering.OpenGL.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import System.IO.Unsafe (unsafePerformIO)


-- Glyph data ----------------------------------------------------------------

data Glyph
  = Glyph
  { glyphTexture :: !(Maybe GL.TextureObject)
  , glyphSize :: !(Int, Int)
  , glyphBearing :: !(Int32, Int32)
  , glyphAdvance :: !Int32
  }


type FontKey = (FilePath, Int)
type FontCache = Map FontKey (Map Char Glyph)


fontCache :: MVar FontCache
fontCache = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE fontCache #-}


-- | Detect the content scale factor (DPI scale) for HiDPI/Retina displays
getContentScale :: IO Float
getContentScale = do
  mWindow <- GLFW.getCurrentContext
  case mWindow of
    Nothing -> pure 1.0 -- No window context, assume scale 1.0
    Just win -> do
      (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
      (winWidth, winHeight) <- GLFW.getWindowSize win
      -- Calculate scale factor from framebuffer vs window size
      -- Use width as primary, fall back to height if width is zero
      let scaleX = if winWidth > 0 then fromIntegral fbWidth / fromIntegral winWidth else 1.0
          scaleY = if winHeight > 0 then fromIntegral fbHeight / fromIntegral winHeight else 1.0
      -- Use the maximum scale factor to ensure crisp rendering
      pure $ max scaleX scaleY


renderTrueTypeText :: FilePath -> Int -> Text -> IO ()
renderTrueTypeText fontPath pixelHeight str
  | T.null str = pure ()
  | otherwise = do
      -- Detect DPI scale factor for HiDPI/Retina displays
      scaleFactor <- getContentScale
      let scaledHeight = round (fromIntegral pixelHeight * scaleFactor)

      glyphs <- getOrLoadFont fontPath scaledHeight
      let fallbackGlyph = Map.lookup '?' glyphs
      GL.texture GL.Texture2D $= GL.Enabled
      oldTexFunc <- get GL.textureFunction
      GL.textureFunction $= Modulate
      GL.preservingMatrix $ do
        -- Scale down by the same factor to maintain logical size
        GL.scale
          (1.0 / realToFrac scaleFactor)
          (1.0 / realToFrac scaleFactor)
          (1.0 :: GL.GLfloat)
        _ <-
          foldM
            (renderGlyph glyphs fallbackGlyph scaledHeight)
            (0, 0)
            (T.unpack str)
        pure ()
      GL.textureFunction $= oldTexFunc
      GL.texture GL.Texture2D $= GL.Disabled


renderGlyph ::
  Map Char Glyph ->
  Maybe Glyph ->
  Int ->
  (Float, Float) ->
  Char ->
  IO (Float, Float)
renderGlyph glyphs fallback lineHeight (penX, penY) = \case
  '\n' ->
    pure (0, penY - fromIntegral lineHeight)
  ch ->
    do
      let glyph = Map.lookup ch glyphs <|> fallback
      case glyph of
        Nothing ->
          pure (penX, penY)
        Just
          Glyph{glyphTexture, glyphSize = (w, h), glyphBearing = (bx, by), glyphAdvance} ->
            do
              let advance = fromIntegral glyphAdvance / 64
                  xpos = penX + fromIntegral bx
                  ypos = penY + fromIntegral by - fromIntegral h
                  width = fromIntegral w
                  height = fromIntegral h
              case glyphTexture of
                Nothing ->
                  pure (penX + advance, penY)
                Just tex -> do
                  textureBinding GL.Texture2D $= Just tex
                  GL.renderPrimitive GL.Quads $ do
                    GL.texCoord $ GL.TexCoord2 0 (0 :: GL.GLfloat)
                    GL.vertex $ GL.Vertex3 (gf xpos) (gf (ypos + height)) 0
                    GL.texCoord $ GL.TexCoord2 0 (1 :: GL.GLfloat)
                    GL.vertex $ GL.Vertex3 (gf xpos) (gf ypos) 0
                    GL.texCoord $ GL.TexCoord2 1 (1 :: GL.GLfloat)
                    GL.vertex $ GL.Vertex3 (gf (xpos + width)) (gf ypos) 0
                    GL.texCoord $ GL.TexCoord2 1 (0 :: GL.GLfloat)
                    GL.vertex $ GL.Vertex3 (gf (xpos + width)) (gf (ypos + height)) 0
                  pure (penX + advance, penY)


-- Font loading --------------------------------------------------------------

getOrLoadFont :: FilePath -> Int -> IO (Map Char Glyph)
getOrLoadFont fontPath pixelHeight =
  modifyMVar fontCache $
    \cache ->
      case Map.lookup key cache of
        Just font ->
          pure (cache, font)
        Nothing -> do
          font <- loadFont fontPath pixelHeight
          pure (Map.insert key font cache, font)
  where
    key = (fontPath, pixelHeight)


loadFont :: FilePath -> Int -> IO (Map Char Glyph)
loadFont fontPath pixelHeight = do
  library <- ft_Init_FreeType
  face <- ft_New_Face library fontPath 0
  ft_Set_Pixel_Sizes face 0 (fromIntegral pixelHeight)
  GL.rowAlignment GL.Unpack $= 1
  glyphs <- forM [0 .. 255] $ loadGlyph face
  ft_Done_Face face
  ft_Done_FreeType library
  let (errs, okGlyphs) = partitionEithers glyphs
  case errs of
    [] -> pure (Map.fromList okGlyphs)
    (err : _) -> ioError (userError err)


loadGlyph :: FT_Face -> Int -> IO (Either String (Char, Glyph))
loadGlyph face codepoint = do
  let ch = toEnum codepoint
  loadResult <-
    try (ft_Load_Char face (fromIntegral codepoint) FT_LOAD_RENDER) ::
      IO (Either FtError ())
  case loadResult of
    Left err ->
      pure . Left $
        "FreeType load error for char " <> show ch <> ": " <> show err
    Right () -> do
      faceRec <- peek face
      glyphSlot <- peek (frGlyph faceRec)
      let bitmap = gsrBitmap glyphSlot
          width = fromIntegral (bWidth bitmap) :: Int
          rows = fromIntegral (bRows bitmap) :: Int
          bearingX = fromIntegral (gsrBitmap_left glyphSlot) :: Int32
          bearingY = fromIntegral (gsrBitmap_top glyphSlot) :: Int32
          advance = fromIntegral (vX (gsrAdvance glyphSlot)) :: Int32
          buffer = bBuffer bitmap

      texture <-
        if width == 0 || rows == 0 || buffer == nullPtr
          then pure Nothing
          else do
            rgbaData <- buildRgbaBuffer width rows buffer
            tex <- GL.genObjectName
            GL.textureBinding GL.Texture2D $= Just tex
            withForeignPtr rgbaData $ \ptr ->
              GL.texImage2D
                GL.Texture2D
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D (fromIntegral width) (fromIntegral rows))
                0
                (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
            GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
            GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
            GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
            pure (Just tex)

      pure $
        Right
          ( ch
          , Glyph
              { glyphTexture = texture
              , glyphSize = (width, rows)
              , glyphBearing = (bearingX, bearingY)
              , glyphAdvance = advance
              }
          )


buildRgbaBuffer :: Int -> Int -> Ptr Word8 -> IO (ForeignPtr Word8)
buildRgbaBuffer width rows src = do
  let pixelCount = width * rows
      byteCount = pixelCount * 4
  dest <- mallocForeignPtrBytes byteCount
  withForeignPtr dest $ \ptr -> do
    forM_ [0 .. pixelCount - 1] $ \i -> do
      alpha <- peekElemOff src i
      let base = i * 4
      pokeElemOff ptr base (255 :: Word8)
      pokeElemOff ptr (base + 1) (255 :: Word8)
      pokeElemOff ptr (base + 2) (255 :: Word8)
      pokeElemOff ptr (base + 3) alpha
  pure dest
