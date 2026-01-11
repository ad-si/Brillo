module Main where

import Brillo.Data.Bitmap
import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Export
import Brillo.Export.Image
import Codec.Picture
import Control.Monad
import Data.Foldable
import Data.Text (pack)
import System.Directory
import System.FilePath
import Text.Printf


-- A variant of 'readImage' which fails with an exception instead of a 'Left'
loadJuicyThrow :: FilePath -> IO DynamicImage
loadJuicyThrow filePath = do
  r <- readImage filePath
  case r of
    Left err -> error err
    Right dynamicImage -> pure dynamicImage


-- A variant of 'readGifImages' which fails with an exception instead of a 'Left'
readGifImagesThrow :: FilePath -> IO [DynamicImage]
readGifImagesThrow filePath = do
  r <- readGifImages filePath
  case r of
    Left err -> error err
    Right dynamicImages -> pure dynamicImages


-- Some versions of JuicyPixels we want to support do not have an Eq instance
-- for 'Image', so we need to compare the individual fields ourselves.
eqImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Bool
eqImage image1 image2 =
  imageWidth image1 == imageWidth image2
    && imageHeight image1 == imageHeight image2
    && imageData image1 == imageData image2


-- Even on the versions of JuicyPixels which do have an Eq instance for
-- 'DynamicImage', we need a version which considers two dynamic images which
-- have the same pixels but use a different format to be equal.
eqDynamicImage :: DynamicImage -> DynamicImage -> Bool
eqDynamicImage dynamicImage1 dynamicImage2 =
  eqImage
    (convertRGBA8 dynamicImage1)
    (convertRGBA8 dynamicImage1)


assertSameImageFiles :: FilePath -> FilePath -> IO ()
assertSameImageFiles filePath1 filePath2 = do
  dynamicImage1 <- loadJuicyThrow filePath1
  dynamicImage2 <- loadJuicyThrow filePath2

  unless (eqDynamicImage dynamicImage1 dynamicImage2) $ do
    error $ "images do not match: " ++ filePath1 ++ ", " ++ filePath2


assertSameGifFiles :: FilePath -> FilePath -> IO ()
assertSameGifFiles filePath1 filePath2 = do
  dynamicImages1 <- readGifImagesThrow filePath1
  dynamicImages2 <- readGifImagesThrow filePath2

  unless (length dynamicImages1 == length dynamicImages2) $ do
    error $
      "gifs have a different number of frames: " ++ filePath1 ++ ", " ++ filePath2
  for_ (zip [1 ..] (zipWith eqDynamicImage dynamicImages1 dynamicImages2)) $ \(i, framesMatch) -> do
    unless framesMatch $ do
      error $
        "animations do not match at frame "
          ++ show i
          ++ ": "
          ++ filePath1
          ++ ", "
          ++ filePath2


-- Validates the given image against an expected image, which is assumed to be
-- a png file found at the same location as the given image, but prefixed with
-- "expected_".
assertSameImageAsExpected :: FilePath -> IO ()
assertSameImageAsExpected filePath = do
  let expectedFilePath = replaceExtension ("expected_" ++ filePath) "png"
  assertSameImageFiles filePath expectedFilePath


-- A wrapper around functions like 'exportPictureToPNG' which also validates
-- the generated image.
exportPictureAndCheck ::
  -- | wrapped export function
  (Size -> Color -> FilePath -> Picture -> IO ()) ->
  -- | width, height in pixels
  Size ->
  -- | background color
  Color ->
  FilePath ->
  Picture ->
  IO ()
exportPictureAndCheck exportPicture size bg filePath picture = do
  exportPicture size bg filePath picture
  assertSameImageAsExpected filePath


-- A wrapper around functions like 'exportPicturesToPNG' which also validates
-- the generated images.
exportPicturesAndCheck ::
  -- | wrapped export function
  (Size -> Color -> FilePath -> Animation -> [Float] -> IO ()) ->
  -- | width, height in pixels
  Size ->
  -- | background color
  Color ->
  FilePath ->
  -- | function that maps from point in time to Picture. analog to Brillo.Animation
  Animation ->
  -- | list of points in time at which to evaluate the animation
  [Float] ->
  IO ()
exportPicturesAndCheck exportPictures size bg filePathPattern animation ts = do
  exportPictures size bg filePathPattern animation ts
  for_ [1 .. length ts] $ \i -> do
    let filePath = printf filePathPattern i
    assertSameImageAsExpected filePath


size :: (Int, Int)
size = (1500, 1000)


shorthand = exportPictureAndCheck exportPictureToPNG size white


main :: IO ()
main = do
  -- @stack test@ sets the working directory to the project root, but all the
  -- filenames are relative to the @test@ folder.
  setCurrentDirectory "test"

  -- Test multiple Brillo features and multiple export formats.
  bmp <- loadBMP "loadme.bmp"
  let pic =
        Pictures
          [ bmp
          , Color red $ Polygon [(-80, 0), (0, 80), (80, 0)]
          , Circle 80
          , Text (pack "text")
          ]
  exportPictureAndCheck
    exportPictureToPNG
    (400, 400)
    white
    "comprehensive.png"
    pic
  exportPictureAndCheck
    exportPictureToBitmap
    (400, 400)
    white
    "comprehensive.bmp"
    pic
  exportPictureAndCheck
    exportPictureToTga
    (400, 400)
    white
    "comprehensive.tga"
    pic
  exportPictureAndCheck
    exportPictureToTiff
    (400, 400)
    white
    "comprehensive.tiff"
    pic
  -- display (InWindow "" (100,80) (0, 0)) white pic
  shorthand "bmp.png" (bmp)
  shorthand "circle.png" (circle 25)
  exportPictureAndCheck
    exportPictureToPNG
    (500, 500)
    white
    "circles.png"
    (Pictures (map circle [0, 10 .. 250]))

  -- Make sure we can export large images. In a previous version, attempting
  -- to export an image larger than the screen resolution WxH resulted in a
  -- scaled down image displayed in the lower-left WxH rectantle, with
  -- transparent pixels everywhere else.
  let wby2 = 1853 / 2
  let hby2 = 1025 / 2
  let p =
        Pictures
          [ Translate wby2 hby2 $ ThickCircle 10 80
          , Translate wby2 hby2 $ ThickCircle 10 80
          , Color blue $ Line [(-wby2, hby2), (wby2, -hby2)]
          , ThickCircle 10 80
          ]
  exportPictureAndCheck exportPictureToPNG (1900, 1050) white "large_image.png" p

  let stack =
        ( Pictures
            [ Color blue $ poly 49 -- 100x100
            , Color red $ poly 30 -- 60x60
            , Color yellow $ poly 20 -- 40x40
            , Color green $ poly 10 -- 20x20
            , Color white $ poly 5 -- 10x10
            ]
        )

  exportPictureAndCheck exportPictureToPNG (10, 10) white "p10.png" stack
  exportPictureAndCheck exportPictureToPNG (20, 20) white "p20.png" stack
  exportPictureAndCheck exportPictureToPNG (40, 40) white "p40.png" stack
  exportPictureAndCheck exportPictureToPNG (60, 60) white "p60.png" stack
  exportPictureAndCheck exportPictureToPNG (100, 100) white "p100.png" stack

  exportPicturesAndCheck
    exportPicturesToPNG
    (1000, 1000)
    white
    "growing_polgons%d.png"
    (Color blue . poly)
    [200, 250 .. 500]
  exportPicturesAndCheck
    exportPicturesToBitmap
    (1000, 1000)
    white
    "growing_polgons%d.bmp"
    (Color blue . poly)
    [200, 250 .. 500]
  exportPicturesAndCheck
    exportPicturesToTga
    (1000, 1000)
    white
    "growing_polgons%d.tga"
    (Color blue . poly)
    [200, 250 .. 500]
  exportPicturesAndCheck
    exportPicturesToTiff
    (1000, 1000)
    white
    "growing_polgons%d.tiff"
    (Color blue . poly)
    [200, 250 .. 500]

  exportPicturesToGif
    10
    LoopingNever
    (1000, 1000)
    red
    "growing_polgons.gif"
    (Color blue . poly)
    [200, 250 .. 500]
  assertSameGifFiles "growing_polgons.gif" "expected_growing_polgons.gif"


textFloats :: [Float]
textFloats = [0, 1 .. 10]


poly :: Float -> Picture
poly l = Polygon [(-l, l), (l, l), (l, -l), (-l, -l)]
