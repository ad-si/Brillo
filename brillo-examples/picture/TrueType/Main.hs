{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Demonstrate rendering TrueType fonts with Brillo.
module Main (main) where

import Brillo
import Control.Exception (bracket, try)
import Control.Monad (foldM, forM)
import Data.Char (ord)
import Data.List (foldl1', isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import FreeType (
  FT_Bitmap (bRows, bWidth),
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
import Foreign (peek)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do
  args <- getArgs
  let fontArg = listToMaybe [a | a <- args, not ("--" `isPrefixOf` a)]
  fontPath <- resolveFont fontArg
  SceneAssets{sceneWindowSize, scenePicture} <- buildScene fontPath
  let window =
        InWindow
          "Brillo TrueType"
          sceneWindowSize
          windowPosition
  display window white scenePicture


windowPosition :: (Int, Int)
windowPosition = (40, 40)


buildScene :: FilePath -> IO SceneAssets
buildScene fontPath = do
  measured <-
    forM textItems $ \item -> do
      bounds <- measureTrueTypeBounds fontPath (tiPixelHeight item) (tiContent item)
      let translatedBounds = translateBounds (tiOffset item) bounds
          picture =
            translate dx dy $
              color (tiColor item) $
                truetypeText fontPath (tiPixelHeight item) (tiContent item)
            where
              (dx, dy) = tiOffset item
      pure (picture, translatedBounds)
  let contentBounds = foldl1' unionBounds (map snd measured)
      padding = 40
      paddedBounds =
        expandBounds
          padding
          padding
          padding
          padding
          contentBounds
      width = boundsWidth paddedBounds
      height = boundsHeight paddedBounds
      centerX = (boundsMinX paddedBounds + boundsMaxX paddedBounds) / 2
      centerY = (boundsMinY paddedBounds + boundsMaxY paddedBounds) / 2
      offsetX = -centerX
      offsetY = -centerY
      background =
        translate centerX centerY $
          color (makeColor 0.08 0.09 0.12 1.0) $
            rectangleSolid width height
      picture =
        translate offsetX offsetY $
          pictures (background : map fst measured)
      widthPixels = max 200 (ceiling width)
      heightPixels = max 200 (ceiling height)
  pure
    SceneAssets
      { sceneWindowSize = (widthPixels, heightPixels)
      , scenePicture = picture
      }


data SceneAssets = SceneAssets
  { sceneWindowSize :: (Int, Int)
  , scenePicture :: Picture
  }


data TextItem = TextItem
  { tiOffset :: (Float, Float)
  , tiColor :: Color
  , tiPixelHeight :: Int
  , tiContent :: Text
  }


textItems :: [TextItem]
textItems =
  [ TextItem
      { tiOffset = (-360, 180)
      , tiColor = makeColor 0.96 0.85 0.45 1.0
      , tiPixelHeight = 56
      , tiContent = "TrueType rendering"
      }
  , TextItem
      { tiOffset = (-420, 60)
      , tiColor = makeColor 0.75 0.95 1.0 1.0
      , tiPixelHeight = 32
      , tiContent = "Brillo now loads glyphs via FreeType."
      }
  , TextItem
      { tiOffset = (-420, 10)
      , tiColor = makeColor 0.85 0.75 1.0 1.0
      , tiPixelHeight = 32
      , tiContent = "Kerning, hinting and newlines all work!"
      }
  , TextItem
      { tiOffset = (-420, -40)
      , tiColor = makeColor 0.8 1.0 0.75 1.0
      , tiPixelHeight = 32
      , tiContent = "Pass a custom font path via CLI arguments."
      }
  , TextItem
      { tiOffset = (-420, -130)
      , tiColor = makeColor 0.95 0.95 0.95 1.0
      , tiPixelHeight = 24
      , tiContent = "Multi-line text is supported:\nLine one\nLine two\nLine three"
      }
  ]


measureTrueTypeBounds :: FilePath -> Int -> Text -> IO Bounds
measureTrueTypeBounds fontPath pixelHeight txt
  | T.null txt = pure zeroBounds
  | otherwise = do
      let lineHeight = fromIntegral pixelHeight :: Float
          lineCount = max 1 (length (T.splitOn "\n" txt))
      bracket ft_Init_FreeType ft_Done_FreeType $ \library ->
        bracket (ft_New_Face library fontPath 0) ft_Done_Face $ \face -> do
          ft_Set_Pixel_Sizes face 0 (fromIntegral pixelHeight)
          (mbBounds, penX, penY, maxPenX, minSeenY, maxSeenY) <-
            foldM
              (accumulate face lineHeight)
              (Nothing, 0, 0, 0, 0, 0)
              (T.unpack txt)
          let maxPenX' = max maxPenX penX
              minSeenY' = min minSeenY penY
              maxSeenY' = max maxSeenY penY
              baseBounds =
                case mbBounds of
                  Nothing ->
                    Bounds
                      { boundsMinX = 0
                      , boundsMaxX = max maxPenX' lineHeight
                      , boundsMinY = negate (fromIntegral lineCount - 1) * lineHeight
                      , boundsMaxY = lineHeight
                      }
                  Just b ->
                    Bounds
                      { boundsMinX = min (boundsMinX b) 0
                      , boundsMaxX = max (boundsMaxX b) (max maxPenX' lineHeight)
                      , boundsMinY = min (boundsMinY b) minSeenY'
                      , boundsMaxY = max (boundsMaxY b) (max maxSeenY' lineHeight)
                      }
              ensuredHeight =
                if boundsHeight baseBounds <= 0
                  then baseBounds{boundsMaxY = boundsMinY baseBounds + lineHeight}
                  else baseBounds
          pure ensuredHeight
  where
    accumulate ::
      FT_Face ->
      Float ->
      (Maybe Bounds, Float, Float, Float, Float, Float) ->
      Char ->
      IO (Maybe Bounds, Float, Float, Float, Float, Float)
    accumulate face lineHeight (mbBounds, penX, penY, maxPenX, minSeenY, maxSeenY) ch
      | ch == '\r' =
          pure (mbBounds, penX, penY, maxPenX, minSeenY, maxSeenY)
      | ch == '\n' =
          let maxPenX' = max maxPenX penX
              maxSeenY' = max maxSeenY penY
              newPenY = penY - lineHeight
              minSeenY' = min minSeenY newPenY
          in  pure (mbBounds, 0, newPenY, maxPenX', minSeenY', maxSeenY')
      | otherwise = do
          loadResult <-
            try (ft_Load_Char face (fromIntegral (ord ch)) FT_LOAD_RENDER) ::
              IO (Either FtError ())
          case loadResult of
            Left _ ->
              pure (mbBounds, penX, penY, maxPenX, minSeenY, maxSeenY)
            Right () -> do
              faceRec <- peek face
              glyphSlot <- peek (frGlyph faceRec)
              let glyphBitmap = gsrBitmap glyphSlot
                  width = fromIntegral (bWidth glyphBitmap) :: Float
                  height = fromIntegral (bRows glyphBitmap) :: Float
                  bearingX = fromIntegral (gsrBitmap_left glyphSlot) :: Float
                  bearingY = fromIntegral (gsrBitmap_top glyphSlot) :: Float
                  xpos = penX + bearingX
                  ypos = penY + bearingY - height
                  top = ypos + height
                  right = xpos + width
                  glyphBounds =
                    if width <= 0 && height <= 0
                      then Nothing
                      else Just Bounds
                        { boundsMinX = xpos
                        , boundsMaxX = right
                        , boundsMinY = ypos
                        , boundsMaxY = top
                        }
                  bounds' =
                    case (mbBounds, glyphBounds) of
                      (Nothing, Nothing) -> Nothing
                      (Just b, Nothing) -> Just b
                      (Nothing, Just g) -> Just g
                      (Just b, Just g) -> Just (unionBounds b g)
                  advance = realToFrac (vX (gsrAdvance glyphSlot)) / 64
                  penX' = penX + advance
                  maxPenX' = max maxPenX penX'
                  minSeenY' = min minSeenY (min ypos penY)
                  maxSeenY' = max maxSeenY (max top penY)
              pure (bounds', penX', penY, maxPenX', minSeenY', maxSeenY')


data Bounds = Bounds
  { boundsMinX :: !Float
  , boundsMaxX :: !Float
  , boundsMinY :: !Float
  , boundsMaxY :: !Float
  }
  deriving (Show)


zeroBounds :: Bounds
zeroBounds = Bounds 0 0 0 0


boundsWidth :: Bounds -> Float
boundsWidth Bounds{boundsMinX, boundsMaxX} = boundsMaxX - boundsMinX


boundsHeight :: Bounds -> Float
boundsHeight Bounds{boundsMinY, boundsMaxY} = boundsMaxY - boundsMinY


unionBounds :: Bounds -> Bounds -> Bounds
unionBounds a b =
  Bounds
    { boundsMinX = min (boundsMinX a) (boundsMinX b)
    , boundsMaxX = max (boundsMaxX a) (boundsMaxX b)
    , boundsMinY = min (boundsMinY a) (boundsMinY b)
    , boundsMaxY = max (boundsMaxY a) (boundsMaxY b)
    }


translateBounds :: (Float, Float) -> Bounds -> Bounds
translateBounds (dx, dy) Bounds{boundsMinX, boundsMaxX, boundsMinY, boundsMaxY} =
  Bounds
    { boundsMinX = boundsMinX + dx
    , boundsMaxX = boundsMaxX + dx
    , boundsMinY = boundsMinY + dy
    , boundsMaxY = boundsMaxY + dy
    }


expandBounds :: Float -> Float -> Float -> Float -> Bounds -> Bounds
expandBounds padLeft padRight padBottom padTop Bounds{boundsMinX, boundsMaxX, boundsMinY, boundsMaxY} =
  Bounds
    { boundsMinX = boundsMinX - padLeft
    , boundsMaxX = boundsMaxX + padRight
    , boundsMinY = boundsMinY - padBottom
    , boundsMaxY = boundsMaxY + padTop
    }


resolveFont :: Maybe FilePath -> IO FilePath
resolveFont (Just fp) = ensureFont fp
resolveFont Nothing = do
  found <- foldM pick Nothing candidateFonts
  case found of
    Just fp -> pure fp
    Nothing -> do
      hPutStrLn stderr $
        unlines
          [ "brillo-truetype: unable to locate a TrueType font."
          , "Tried the following paths:"
          , unlines (map ("  • " <>) candidateFonts)
          , "Pass a font explicitly: stack run brillo-truetype -- /path/to/font.ttf"
          ]
      exitFailure
  where
    pick acc candidate =
      case acc of
        Just _ -> pure acc
        Nothing -> do
          exists <- doesFileExist candidate
          pure $ if exists then Just candidate else Nothing


ensureFont :: FilePath -> IO FilePath
ensureFont fp = do
  exists <- doesFileExist fp
  if exists
    then pure fp
    else do
      hPutStrLn stderr $ "brillo-truetype: font file not found: " <> fp
      exitFailure


candidateFonts :: [FilePath]
candidateFonts =
  [ "/System/Library/Fonts/Supplemental/Arial.ttf"
  , "/Library/Fonts/Arial.ttf"
  , "/System/Library/Fonts/Supplemental/Helvetica.ttc"
  , "/System/Library/Fonts/Supplemental/Tahoma.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  , "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
  , "C:\\\\Windows\\\\Fonts\\\\arial.ttf"
  , "C:\\\\Windows\\\\Fonts\\\\segoeui.ttf"
  ]
