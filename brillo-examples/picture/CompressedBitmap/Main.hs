{-| Demo of GPU-compressed (DXT1 / BC1) textures.

Loads an uncompressed BMP, encodes it to DXT1 in pure Haskell, and shows
both side by side. The left tile lives in VRAM as RGBA8 (4 B/pixel); the
right tile is uploaded via @glCompressedTexImage2D@ as DXT1 (0.5 B/pixel)
and decoded by the GPU at sample time.
-}
module Main where

import Brillo
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)


defaultBmpPath :: FilePath
defaultBmpPath = "brillo-examples/picture/Bitmap/lena-101x101.bmp"


main :: IO ()
main = do
  args <- getArgs
  let path = case args of
        [p] -> p
        _ -> defaultBmpPath

  picture <- loadBMP path
  case picture of
    Bitmap bmpData -> run path bmpData
    _ -> error "loadBMP did not return a Bitmap"


run :: FilePath -> BitmapData -> IO ()
run path bmpData = do
  let (srcW, srcH) = bitmapSize bmpData
      -- DXT1 blocks are 4×4 pixels, so crop to a multiple of 4.
      w = srcW - srcW `mod` 4
      h = srcH - srcH `mod` 4
      compressedBytes = encodeDXT1 bmpData w h
      compressedPic =
        compressedBitmapOfByteString
          w
          h
          TopToBottom
          compressedRGB_S3TC_DXT1
          compressedBytes
          True
      croppedUncompressed = BitmapSection (Rectangle (0, 0) (w, h)) bmpData

      gap = 20 :: Int
      windowW = w * 2 + gap * 3
      windowH = h + gap * 2
      offset = fromIntegral w / 2 + fromIntegral gap / 2 :: Float

  putStrLn $
    "Source: "
      <> path
      <> " ("
      <> show srcW
      <> "×"
      <> show srcH
      <> "), DXT1 payload: "
      <> show (BS.length compressedBytes)
      <> " B for "
      <> show w
      <> "×"
      <> show h
      <> " (uncompressed would be "
      <> show (w * h * 4)
      <> " B)"

  display
    ( InWindow
        (T.pack "Compressed Textures (RGBA8 vs DXT1)")
        (windowW, windowH)
        (40, 40)
    )
    (greyN 0.15)
    ( Pictures
        [ Translate (-offset) 0 croppedUncompressed
        , Translate offset 0 compressedPic
        ]
    )


-- DXT1 (BC1) encoder ---------------------------------------------------------
-- 4×4 pixel blocks → 8 bytes each; ~6:1 compression vs RGBA8.

encodeDXT1 :: BitmapData -> Int -> Int -> ByteString
encodeDXT1 bmpData w h =
  unsafePerformIO $
    withForeignPtr (bitmapPointer bmpData) $ \ptr -> do
      let (srcW, srcH) = bitmapSize bmpData
      builder <-
        mconcat
          <$> sequence
            [ encodeBlock ptr srcW srcH bx by
            | by <- [0, 4 .. h - 1]
            , bx <- [0, 4 .. w - 1]
            ]
      pure $ BSL.toStrict (BSB.toLazyByteString builder)


{-| Read one 4×4 block of RGB triples from the source BMP (which is stored
bottom-to-top) and emit its 8-byte DXT1 representation.
-}
encodeBlock :: Ptr Word8 -> Int -> Int -> Int -> Int -> IO BSB.Builder
encodeBlock ptr srcW srcH bx by = do
  pixels <-
    traverse readPixel [(bx + dx, by + dy) | dy <- [0 .. 3], dx <- [0 .. 3]]
  pure (blockBytes pixels)
  where
    readPixel (x, y) = do
      let sy = srcH - 1 - y -- BMP is bottom-to-top; flip to top-to-bottom.
          off = (sy * srcW + x) * 4
      r <- peekByteOff ptr off :: IO Word8
      g <- peekByteOff ptr (off + 1) :: IO Word8
      b <- peekByteOff ptr (off + 2) :: IO Word8
      pure (r, g, b)


{-| Given 16 RGB pixels in row-major order, pick endpoints by luminance,
snap each pixel to its nearest palette entry, and emit 8 bytes.
-}
blockBytes :: [(Word8, Word8, Word8)] -> BSB.Builder
blockBytes pixels =
  let
    (cMin, cMax) = endpointsByLuminance pixels
    (c0Initial, c1Initial) = (toRGB565 cMax, toRGB565 cMin)
    -- Ensure 4-color mode by keeping c0 > c1 as uint16.
    (c0, c1) =
      if c0Initial >= c1Initial
        then (c0Initial, c1Initial)
        else (c1Initial, c0Initial)
    p0 = fromRGB565 c0
    p1 = fromRGB565 c1
    p2 = lerp23 p0 p1
    p3 = lerp23 p1 p0
    palette = [p0, p1, p2, p3]
    indices = zip [0 :: Int ..] (map (nearestIndex palette) pixels)
    packed =
      List.foldl'
        (\acc (i, idx) -> acc .|. (fromIntegral idx `shiftL` (2 * i)))
        (0 :: Word32)
        indices
  in
    BSB.word16LE c0 <> BSB.word16LE c1 <> BSB.word32LE packed


-- | Single pass over the pixels picking the darkest and brightest by luminance.
endpointsByLuminance ::
  [(Word8, Word8, Word8)] ->
  ((Word8, Word8, Word8), (Word8, Word8, Word8))
endpointsByLuminance pixels =
  let decorated = [(luminance p, p) | p <- pixels]
      pick cmp = snd . foldr1 (\a b -> if cmp (fst a) (fst b) then a else b)
  in  (pick (<) decorated, pick (>) decorated)


toRGB565 :: (Word8, Word8, Word8) -> Word16
toRGB565 (r, g, b) =
  ((fromIntegral r `shiftR` 3) `shiftL` 11)
    .|. ((fromIntegral g `shiftR` 2) `shiftL` 5)
    .|. (fromIntegral b `shiftR` 3)


fromRGB565 :: Word16 -> (Word8, Word8, Word8)
fromRGB565 c =
  let r5 = fromIntegral ((c `shiftR` 11) .&. 0x1F)
      g6 = fromIntegral ((c `shiftR` 5) .&. 0x3F)
      b5 = fromIntegral (c .&. 0x1F)
  in  ( (r5 `shiftL` 3) .|. (r5 `shiftR` 2)
      , (g6 `shiftL` 2) .|. (g6 `shiftR` 4)
      , (b5 `shiftL` 3) .|. (b5 `shiftR` 2)
      )


-- | Integer (2/3, 1/3) interpolation between two endpoint colors.
lerp23 ::
  (Word8, Word8, Word8) ->
  (Word8, Word8, Word8) ->
  (Word8, Word8, Word8)
lerp23 (r0, g0, b0) (r1, g1, b1) =
  (mix r0 r1, mix g0 g1, mix b0 b1)
  where
    mix a b =
      fromIntegral
        ((2 * fromIntegral a + fromIntegral b + 1 :: Int) `div` 3)


-- | BT.601 luma weights × 100 (integer-only).
luminance :: (Word8, Word8, Word8) -> Int
luminance (r, g, b) =
  fromIntegral r * 30 + fromIntegral g * 59 + fromIntegral b * 11


nearestIndex :: [(Word8, Word8, Word8)] -> (Word8, Word8, Word8) -> Int
nearestIndex palette c =
  fst $
    foldr1
      (\(i, d) (j, d') -> if d <= d' then (i, d) else (j, d'))
      (zip [0 ..] (map (squaredDist c) palette))


squaredDist ::
  (Word8, Word8, Word8) -> (Word8, Word8, Word8) -> Int
squaredDist (r1, g1, b1) (r2, g2, b2) =
  let s a b = let x = fromIntegral a - fromIntegral b :: Int in x * x
  in  s r1 r2 + s g1 g2 + s b1 b2
