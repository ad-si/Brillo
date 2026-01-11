{-# LANGUAGE OverloadedStrings #-}

{-| Example demonstrating how to export Brillo pictures to various image formats
  using the brillo-export library.

  This example creates several geometric shapes and exports them as:
  - PNG files
  - BMP files
  - TGA files
  - TIFF files
  - An animated GIF
-}
module Main where

import Brillo
import Brillo.Export
import System.Directory (createDirectoryIfMissing)


-- | A simple picture with colored shapes
samplePicture :: Picture
samplePicture =
  Pictures
    [ Color red $ Translate (-50) 50 $ Circle 30
    , Color green $ Translate 50 50 $ rectangleSolid 60 60
    , Color blue $ Translate 0 (-50) $ Polygon [(-30, 0), (30, 0), (0, 50)]
    , Color orange $ Text "Brillo"
    ]


-- | A fractal tree for more complex visualization
tree :: Float -> Float -> Picture
tree width depth
  | depth < 1 = Blank
  | otherwise =
      Pictures
        [ Line [(0, 0), (0, width)]
        , Translate 0 width $ Rotate 30 $ tree (width * 0.7) (depth - 1)
        , Translate 0 width $ Rotate (-30) $ tree (width * 0.7) (depth - 1)
        ]


-- | Generate an animation frame
animationFrame :: Float -> Picture
animationFrame t =
  Pictures
    [ Color (makeColor r g b 1.0) $ Rotate (t * 10) $ rectangleSolid size size
    , Color (makeColor g b r 1.0) $ Rotate (-t * 15) $ Circle (size / 2)
    ]
  where
    size = 100 + 50 * sin (t / 10)
    r = (sin t + 1) / 2
    g = (sin (t + 2) + 1) / 2
    b = (sin (t + 4) + 1) / 2


main :: IO ()
main = do
  putStrLn "Exporting Brillo pictures to various formats..."

  -- Create output directory if it doesn't exist
  createDirectoryIfMissing True "output"

  -- Export simple picture to PNG
  putStrLn "Exporting to PNG..."
  exportPictureToPNG (400, 400) white "output/sample.png" samplePicture

  -- Export to BMP
  putStrLn "Exporting to BMP..."
  exportPictureToBitmap (400, 400) white "output/sample.bmp" samplePicture

  -- Export to TGA
  putStrLn "Exporting to TGA..."
  exportPictureToTga (400, 400) white "output/sample.tga" samplePicture

  -- Export to TIFF
  putStrLn "Exporting to TIFF..."
  exportPictureToTiff (400, 400) white "output/sample.tiff" samplePicture

  -- Export tree to PNG with different background
  putStrLn "Exporting tree to PNG..."
  exportPictureToPNG (500, 500) (greyN 0.1) "output/tree.png" $
    Color green $
      tree 80 8

  -- Export multiple pictures as animated GIF
  putStrLn "Exporting animated GIF..."
  let frames = [0, 5 .. 355] -- 72 frames
      delay = 5 -- 50ms delay between frames (20 fps)
  exportPicturesToGif
    delay
    LoopingForever
    (400, 400)
    white
    "output/animation.gif"
    animationFrame
    frames

  putStrLn "\nExport complete:"
  putStrLn "- output/sample.png     (PNG format)"
  putStrLn "- output/sample.bmp     (Bitmap format)"
  putStrLn "- output/sample.tga     (TGA format)"
  putStrLn "- output/sample.tiff    (TIFF format)"
  putStrLn "- output/tree.png       (Fractal tree)"
  putStrLn "- output/animation.gif  (Animated GIF)"
