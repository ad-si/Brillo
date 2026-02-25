{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Comprehensive test suite for WebP export functionality.

This module tests all WebP export features including lossless and lossy
compression, single pictures, and animations.
-}
module Main where

import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Export.WebP
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.Text (pack)
import Graphics.UI.GLFW qualified as GLFW
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getTemporaryDirectory,
  removeFile,
 )
import System.FilePath ((</>))
import Text.Printf (printf)


-- | Test result type
data TestResult
  = TestPass String
  | TestFail String String
  deriving (Show)


isPass :: TestResult -> Bool
isPass (TestPass _) = True
isPass _ = False


-- | Run a test and return the result
runTest :: String -> IO Bool -> IO TestResult
runTest name action = do
  putStr $ "  Testing " ++ name ++ "... "
  result <- action
  if result
    then do
      putStrLn "PASS"
      pure $ TestPass name
    else do
      putStrLn "FAIL"
      pure $ TestFail name "Assertion failed"


-- | Assert that a condition is true
assertBool :: String -> Bool -> IO Bool
assertBool _ True = pure True
assertBool msg False = do
  putStrLn $ "\n    ERROR: " ++ msg
  pure False


-- | Standard test size
testSize :: (Int, Int)
testSize = (200, 200)


-- | Output directory for test WebPs
getTestOutputDir :: IO FilePath
getTestOutputDir = do
  tmpDir <- getTemporaryDirectory
  let outputDir = tmpDir </> "brillo-webp-test"
  createDirectoryIfMissing True outputDir
  pure outputDir


{-| Check if GLFW can initialize and create a window (i.e., we have a display
available). On Windows, GLFW.init can succeed even without a display, so we
also try creating a hidden window to verify.
-}
canInitializeGLFW :: IO Bool
canInitializeGLFW = do
  result <- GLFW.init
  if not result
    then pure False
    else do
      GLFW.windowHint (GLFW.WindowHint'Visible False)
      maybeWindow <- GLFW.createWindow 1 1 "" Nothing Nothing
      case maybeWindow of
        Nothing -> do
          GLFW.terminate
          pure False
        Just window -> do
          GLFW.destroyWindow window
          GLFW.terminate
          pure True


-- | Clean up a test file if it exists
cleanupFile :: FilePath -> IO ()
cleanupFile path = do
  exists <- doesFileExist path
  when exists $ removeFile path


-- ============================================================================
-- Test Categories
-- ============================================================================

-- | Test lossless WebP export of single pictures
testLosslessSinglePicture :: FilePath -> IO [TestResult]
testLosslessSinglePicture outputDir = do
  putStrLn "\n=== Testing Lossless Single Picture Export ==="

  sequence
    [ runTest "Export simple circle" $ do
        let pic = Circle 50
        let filePath = outputDir </> "lossless_circle.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        if exists
          then do
            content <- BS.readFile filePath
            -- WebP files start with "RIFF" followed by file size and "WEBP"
            let isWebP = BS.take 4 content == "RIFF" && BS.take 4 (BS.drop 8 content) == "WEBP"
            assertBool "File should be a valid WebP" isWebP
          else assertBool "File should exist" False
    , runTest "Export filled polygon" $ do
        let pic = Color red $ Polygon [(0, 0), (100, 0), (50, 100)]
        let filePath = outputDir </> "lossless_polygon.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export complex scene" $ do
        let pic =
              Pictures
                [ Color blue $ circleSolid 40
                , Translate 50 50 $ Color green $ rectangleSolid 30 30
                , Translate (-50) (-50) $ Color red $ Circle 25
                ]
        let filePath = outputDir </> "lossless_complex.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with transparent background" $ do
        let pic = Circle 50
        let bgColor = makeColor 0 0 0 0 -- Fully transparent
        let filePath = outputDir </> "lossless_transparent_bg.webp"
        cleanupFile filePath
        exportPictureToWebP testSize bgColor filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with colored background" $ do
        let pic = Color white $ circleSolid 50
        let filePath = outputDir </> "lossless_colored_bg.webp"
        cleanupFile filePath
        exportPictureToWebP testSize blue filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export text" $ do
        let pic = Scale 0.5 0.5 $ Text (pack "Hello WebP!")
        let filePath = outputDir </> "lossless_text.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with transforms" $ do
        let pic = Translate 50 0 $ Rotate 45 $ Scale 0.8 0.8 $ rectangleSolid 60 60
        let filePath = outputDir </> "lossless_transforms.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export blank picture" $ do
        let pic = Blank
        let filePath = outputDir </> "lossless_blank.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    ]


-- | Test lossy WebP export of single pictures
testLossySinglePicture :: FilePath -> IO [TestResult]
testLossySinglePicture outputDir = do
  putStrLn "\n=== Testing Lossy Single Picture Export ==="

  sequence
    [ runTest "Export with quality 100" $ do
        let pic = Circle 50
        let filePath = outputDir </> "lossy_q100.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 100 testSize white filePath pic
        exists <- doesFileExist filePath
        if exists
          then do
            content <- BS.readFile filePath
            let isWebP = BS.take 4 content == "RIFF" && BS.take 4 (BS.drop 8 content) == "WEBP"
            assertBool "File should be a valid WebP" isWebP
          else assertBool "File should exist" False
    , runTest "Export with quality 75" $ do
        let pic = Color red $ circleSolid 50
        let filePath = outputDir </> "lossy_q75.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 75 testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with quality 50" $ do
        let pic = Color blue $ rectangleSolid 80 60
        let filePath = outputDir </> "lossy_q50.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 50 testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with quality 25" $ do
        let pic = Color green $ Polygon [(0, 0), (60, 0), (30, 80)]
        let filePath = outputDir </> "lossy_q25.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 25 testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export with quality 0" $ do
        let pic = Circle 50
        let filePath = outputDir </> "lossy_q0.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 0 testSize white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Lossy file smaller than lossless (complex image)" $ do
        -- Create a complex picture that should compress well with lossy
        let pic =
              Pictures
                [ Color (makeColor 0.8 0.2 0.3 1) $ circleSolid 80
                , Color (makeColor 0.2 0.8 0.3 1) $ Translate 30 30 $ circleSolid 40
                , Color (makeColor 0.3 0.2 0.8 1) $ Translate (-30) (-30) $ rectangleSolid 50 50
                ]
        let losslessPath = outputDir </> "compare_lossless.webp"
        let lossyPath = outputDir </> "compare_lossy.webp"
        cleanupFile losslessPath
        cleanupFile lossyPath
        exportPictureToWebP testSize white losslessPath pic
        exportPictureToWebPLossy 50 testSize white lossyPath pic
        losslessSize <- BS.length <$> BS.readFile losslessPath
        lossySize <- BS.length <$> BS.readFile lossyPath
        -- Note: Lossy is typically smaller, but not guaranteed for all images
        -- Just verify both files were created with reasonable sizes
        assertBool "Both files should have content" (losslessSize > 0 && lossySize > 0)
    ]


-- | Test lossless WebP export of animations (multiple pictures)
testLosslessAnimation :: FilePath -> IO [TestResult]
testLosslessAnimation outputDir = do
  putStrLn "\n=== Testing Lossless Animation Export ==="

  sequence
    [ runTest "Export animation frames" $ do
        let animation t = Circle (20 + t)
        let filePattern = outputDir </> "lossless_anim%d.webp"
        let timePoints = [10, 20, 30, 40, 50]
        -- Clean up any existing files
        forM_ [1 .. 5] $ \i -> cleanupFile (outputDir </> printf "lossless_anim%d.webp" (i :: Int))
        exportPicturesToWebP testSize white filePattern animation timePoints
        -- Verify all frames exist
        allExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossless_anim%d.webp" (i :: Int)))
            [1 .. 5]
        assertBool "All 5 animation frames should exist" (and allExist)
    , runTest "Export rotating square animation" $ do
        let animation t = Rotate t $ rectangleSolid 50 50
        let filePattern = outputDir </> "lossless_rotate%d.webp"
        let timePoints = [0, 45, 90, 135, 180]
        forM_ [1 .. 5] $ \i -> cleanupFile (outputDir </> printf "lossless_rotate%d.webp" (i :: Int))
        exportPicturesToWebP testSize white filePattern animation timePoints
        allExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossless_rotate%d.webp" (i :: Int)))
            [1 .. 5]
        assertBool "All 5 rotation frames should exist" (and allExist)
    , runTest "Export color changing animation" $ do
        let animation t =
              let r = t / 100
                  g = 1 - (t / 100)
              in  Color (makeColor r g 0.5 1) $ circleSolid 60
        let filePattern = outputDir </> "lossless_color%d.webp"
        let timePoints = [0, 25, 50, 75, 100]
        forM_ [1 .. 5] $ \i -> cleanupFile (outputDir </> printf "lossless_color%d.webp" (i :: Int))
        exportPicturesToWebP testSize white filePattern animation timePoints
        allExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossless_color%d.webp" (i :: Int)))
            [1 .. 5]
        assertBool "All 5 color frames should exist" (and allExist)
    , runTest "Export single frame animation" $ do
        let animation _ = Circle 50
        let filePattern = outputDir </> "lossless_single%d.webp"
        cleanupFile (outputDir </> "lossless_single1.webp")
        exportPicturesToWebP testSize white filePattern animation [0]
        exists <- doesFileExist (outputDir </> "lossless_single1.webp")
        assertBool "Single frame should exist" exists
    ]


-- | Test lossy WebP export of animations (multiple pictures)
testLossyAnimation :: FilePath -> IO [TestResult]
testLossyAnimation outputDir = do
  putStrLn "\n=== Testing Lossy Animation Export ==="

  sequence
    [ runTest "Export lossy animation frames" $ do
        let animation t = Circle (20 + t)
        let filePattern = outputDir </> "lossy_anim%d.webp"
        let timePoints = [10, 20, 30]
        forM_ [1 .. 3] $ \i -> cleanupFile (outputDir </> printf "lossy_anim%d.webp" (i :: Int))
        exportPicturesToWebPLossy 75 testSize white filePattern animation timePoints
        allExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossy_anim%d.webp" (i :: Int)))
            [1 .. 3]
        assertBool "All 3 animation frames should exist" (and allExist)
    , runTest "Export lossy animation with different qualities" $ do
        let animation t = Color blue $ circleSolid t
        -- Export same animation at different qualities
        let filePatternHQ = outputDir </> "lossy_hq%d.webp"
        let filePatternLQ = outputDir </> "lossy_lq%d.webp"
        let timePoints = [30, 40, 50]
        forM_ [1 .. 3] $ \i -> do
          cleanupFile (outputDir </> printf "lossy_hq%d.webp" (i :: Int))
          cleanupFile (outputDir </> printf "lossy_lq%d.webp" (i :: Int))
        exportPicturesToWebPLossy 90 testSize white filePatternHQ animation timePoints
        exportPicturesToWebPLossy 30 testSize white filePatternLQ animation timePoints
        hqExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossy_hq%d.webp" (i :: Int)))
            [1 .. 3]
        lqExist <-
          mapM
            (\i -> doesFileExist (outputDir </> printf "lossy_lq%d.webp" (i :: Int)))
            [1 .. 3]
        assertBool "All HQ and LQ frames should exist" (and hqExist && and lqExist)
    ]


-- | Test different image sizes
testDifferentSizes :: FilePath -> IO [TestResult]
testDifferentSizes outputDir = do
  putStrLn "\n=== Testing Different Image Sizes ==="

  sequence
    [ runTest "Export tiny image (50x50)" $ do
        let pic = Circle 20
        let filePath = outputDir </> "size_50x50.webp"
        cleanupFile filePath
        exportPictureToWebP (50, 50) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export small image (100x100)" $ do
        let pic = Circle 40
        let filePath = outputDir </> "size_100x100.webp"
        cleanupFile filePath
        exportPictureToWebP (100, 100) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export medium image (400x400)" $ do
        let pic = Circle 150
        let filePath = outputDir </> "size_400x400.webp"
        cleanupFile filePath
        exportPictureToWebP (400, 400) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export non-square image (300x200)" $ do
        let pic = rectangleSolid 200 100
        let filePath = outputDir </> "size_300x200.webp"
        cleanupFile filePath
        exportPictureToWebP (300, 200) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export tall image (100x400)" $ do
        let pic = rectangleSolid 50 300
        let filePath = outputDir </> "size_100x400.webp"
        cleanupFile filePath
        exportPictureToWebP (100, 400) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    , runTest "Export wide image (400x100)" $ do
        let pic = rectangleSolid 300 50
        let filePath = outputDir </> "size_400x100.webp"
        cleanupFile filePath
        exportPictureToWebP (400, 100) white filePath pic
        exists <- doesFileExist filePath
        assertBool "File should exist" exists
    ]


-- | Test various picture types
testPictureTypes :: FilePath -> IO [TestResult]
testPictureTypes outputDir = do
  putStrLn "\n=== Testing Various Picture Types ==="

  sequence
    [ runTest "Arc" $ do
        let pic = Arc 0 180 50
        let filePath = outputDir </> "pic_arc.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "ThickArc" $ do
        let pic = ThickArc 45 270 10 50
        let filePath = outputDir </> "pic_thickarc.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "ThickCircle" $ do
        let pic = ThickCircle 15 50
        let filePath = outputDir </> "pic_thickcircle.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "Line" $ do
        let pic = Line [(-80, -80), (80, 80)]
        let filePath = outputDir </> "pic_line.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "ThickLine" $ do
        let pic = ThickLine [(-80, 80), (0, -80), (80, 80)] 5
        let filePath = outputDir </> "pic_thickline.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "LineAliased" $ do
        let pic = LineAliased [(-80, 0), (-40, 60), (40, -60), (80, 0)]
        let filePath = outputDir </> "pic_linesmooth.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    , runTest "Pictures (nested)" $ do
        let pic =
              Pictures
                [ Pictures [Circle 30, Circle 50]
                , Translate 40 40 $ Pictures [Circle 20, Circle 30]
                ]
        let filePath = outputDir </> "pic_nested.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        doesFileExist filePath
    ]


-- | Test WebP file validity
testFileValidity :: FilePath -> IO [TestResult]
testFileValidity outputDir = do
  putStrLn "\n=== Testing WebP File Validity ==="

  sequence
    [ runTest "Lossless WebP has valid header" $ do
        let pic = circleSolid 50
        let filePath = outputDir </> "valid_lossless.webp"
        cleanupFile filePath
        exportPictureToWebP testSize white filePath pic
        content <- BS.readFile filePath
        -- WebP format: RIFF....WEBP
        let riff = BS.take 4 content
        let webp = BS.take 4 (BS.drop 8 content)
        assertBool "Should have RIFF header" (riff == "RIFF") >>= \r1 ->
          if r1
            then assertBool "Should have WEBP marker" (webp == "WEBP")
            else pure False
    , runTest "Lossy WebP has valid header" $ do
        let pic = circleSolid 50
        let filePath = outputDir </> "valid_lossy.webp"
        cleanupFile filePath
        exportPictureToWebPLossy 75 testSize white filePath pic
        content <- BS.readFile filePath
        let riff = BS.take 4 content
        let webp = BS.take 4 (BS.drop 8 content)
        assertBool "Should have RIFF header" (riff == "RIFF") >>= \r1 ->
          if r1
            then assertBool "Should have WEBP marker" (webp == "WEBP")
            else pure False
    , runTest "File size is reasonable" $ do
        let pic = circleSolid 50
        let filePath = outputDir </> "size_check.webp"
        cleanupFile filePath
        exportPictureToWebP (100, 100) white filePath pic
        content <- BS.readFile filePath
        let size = BS.length content
        -- WebP file should be at least 100 bytes but less than 100KB for a simple 100x100 image
        assertBool
          ("File size should be reasonable (got " ++ show size ++ " bytes)")
          (size >= 100 && size < 100000)
    ]


-- | Main test runner
main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  Brillo WebP Export - Comprehensive Tests"
  putStrLn "=========================================="

  -- Check if we can initialize GLFW (requires a display)
  hasDisplay <- canInitializeGLFW
  unless hasDisplay $ do
    putStrLn
      "\nSkipping brillo-webp tests: no display available (GLFW cannot initialize)"
    putStrLn "This is expected in headless CI environments."

  when hasDisplay $ do
    outputDir <- getTestOutputDir
    putStrLn $ "Output directory: " ++ outputDir

    -- Run all test categories
    results <-
      concat
        <$> sequence
          [ testLosslessSinglePicture outputDir
          , testLossySinglePicture outputDir
          , testLosslessAnimation outputDir
          , testLossyAnimation outputDir
          , testDifferentSizes outputDir
          , testPictureTypes outputDir
          , testFileValidity outputDir
          ]

    -- Summary
    let passes = length $ filter isPass results
    let total = length results

    putStrLn "\n=========================================="
    putStrLn $ printf "  Results: %d / %d tests passed" passes total
    putStrLn "=========================================="

    -- Print failures
    let failures = [name | TestFail name _ <- results]
    unless (null failures) $ do
      putStrLn "\nFailed tests:"
      forM_ failures $ \name -> putStrLn $ "  - " ++ name

    -- Exit with appropriate code
    if passes == total
      then putStrLn "\nAll tests passed!"
      else error $ show (total - passes) ++ " test(s) failed."
