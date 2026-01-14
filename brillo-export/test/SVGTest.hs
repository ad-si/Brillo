{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Comprehensive test suite for SVG export functionality.

This module tests all Picture primitives and SVG features to ensure
complete coverage of the SVG export functionality.
-}
module Main where

import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Export.SVG
import Control.Monad (forM_, unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getTemporaryDirectory,
 )
import System.Exit (exitFailure, exitSuccess)
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


-- | Assert that text contains a substring
assertContains :: Text -> Text -> IO Bool
assertContains haystack needle =
  assertBool
    ("Expected to find: " ++ T.unpack needle)
    (needle `T.isInfixOf` haystack)


-- | Assert that text contains all substrings
assertContainsAll :: Text -> [Text] -> IO Bool
assertContainsAll haystack needles = do
  results <- mapM (assertContains haystack) needles
  pure $ and results


-- | Standard test size
testSize :: (Int, Int)
testSize = (400, 400)


-- | Output directory for test SVGs
getTestOutputDir :: IO FilePath
getTestOutputDir = do
  tmpDir <- getTemporaryDirectory
  let outputDir = tmpDir </> "brillo-svg-test"
  createDirectoryIfMissing True outputDir
  pure outputDir


-- ============================================================================
-- Test Categories
-- ============================================================================

-- | Test basic SVG document structure
testDocumentStructure :: IO [TestResult]
testDocumentStructure = do
  putStrLn "\n=== Testing SVG Document Structure ==="

  sequence
    [ runTest "SVG header" $ do
        let svg = pictureToSVGDoc (100, 100) white Blank
        assertContainsAll
          svg
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , "<svg xmlns=\"http://www.w3.org/2000/svg\""
          , "xmlns:xlink=\"http://www.w3.org/1999/xlink\""
          , "width=\"100\""
          , "height=\"100\""
          , "viewBox=\"0 0 100 100\""
          , "</svg>"
          ]
    , runTest "SVG dimensions" $ do
        let svg = pictureToSVGDoc (800, 600) white Blank
        assertContainsAll
          svg
          [ "width=\"800\""
          , "height=\"600\""
          , "viewBox=\"0 0 800 600\""
          ]
    , runTest "Background color white" $ do
        let svg = pictureToSVGDoc (100, 100) white Blank
        assertContains svg "fill=\"rgb(255,255,255)\""
    , runTest "Background color black" $ do
        let svg = pictureToSVGDoc (100, 100) black Blank
        assertContains svg "fill=\"rgb(0,0,0)\""
    , runTest "Background color red" $ do
        let svg = pictureToSVGDoc (100, 100) red Blank
        assertContains svg "fill=\"rgb(255,0,0)\""
    , runTest "Background color with alpha" $ do
        let svg = pictureToSVGDoc (100, 100) (makeColor 1 0 0 0.5) Blank
        assertContains svg "rgba(255,0,0,0.500)"
    ]


-- | Test Blank picture
testBlank :: IO [TestResult]
testBlank = do
  putStrLn "\n=== Testing Blank Picture ==="

  sequence
    [ runTest "Blank produces no content" $ do
        let svg = renderPictureToSVG testSize white Blank
        -- Should only have the background rect and transform wrappers, no actual shapes
        _ <- assertBool "Blank should have no circles" (not $ T.isInfixOf "<circle" svg)
        _ <-
          assertBool "Blank should have no polygons" (not $ T.isInfixOf "<polygon" svg)
        assertBool "Blank should have no polylines" (not $ T.isInfixOf "<polyline" svg)
    ]


-- | Test Polygon primitive
testPolygon :: IO [TestResult]
testPolygon = do
  putStrLn "\n=== Testing Polygon Primitive ==="

  sequence
    [ runTest "Simple triangle" $ do
        let pic = Polygon [(0, 0), (100, 0), (50, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<polygon", "points="]
    , runTest "Square polygon" $ do
        let pic = Polygon [(-50, -50), (50, -50), (50, 50), (-50, 50)]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polygon"
    , runTest "Empty polygon" $ do
        let pic = Polygon []
        let svg = pictureToSVGDoc testSize white pic
        -- Empty polygon should not produce polygon element
        pure $ not (T.isInfixOf "<polygon" svg)
    , runTest "Polygon with color" $ do
        let pic = Color blue $ Polygon [(0, 0), (100, 0), (50, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "fill=\"rgb(0,0,255)\""
    , runTest "Polygon with alpha" $ do
        let pic = Color (makeColor 1 0 0 0.5) $ Polygon [(0, 0), (100, 0), (50, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["rgba(255,0,0,0.500)", "fill-opacity"]
    , runTest "Complex polygon (star)" $ do
        let starPoints =
              [ (0, 100)
              , (22, 31)
              , (95, 31)
              , (36, -12)
              , (59, -81)
              , (0, -38)
              , (-59, -81)
              , (-36, -12)
              , (-95, 31)
              , (-22, 31)
              ]
        let pic = Polygon starPoints
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polygon"
    ]


-- | Test Line primitives
testLine :: IO [TestResult]
testLine = do
  putStrLn "\n=== Testing Line Primitives ==="

  sequence
    [ runTest "Simple line" $ do
        let pic = Line [(0, 0), (100, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polyline"
    , runTest "Line with multiple points" $ do
        let pic = Line [(0, 0), (50, 100), (100, 0), (150, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<polyline", "points=", "fill=\"none\"", "stroke="]
    , runTest "ThickLine" $ do
        let pic = ThickLine [(0, 0), (100, 100)] 5.0
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<polyline", "stroke-width=\"5"]
    , runTest "ThickLine with varying thickness" $ do
        let pic = ThickLine [(0, 0), (100, 100)] 10.0
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "stroke-width=\"10"
    , runTest "Line with color" $ do
        let pic = Color green $ Line [(0, 0), (100, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "stroke=\"rgb(0,255,0)\""
    , runTest "Empty line" $ do
        let pic = Line []
        let svg = pictureToSVGDoc testSize white pic
        pure $ not (T.isInfixOf "<polyline" svg)
    , runTest "Single point line" $ do
        let pic = Line [(50, 50)]
        let svg = pictureToSVGDoc testSize white pic
        pure $ not (T.isInfixOf "<polyline" svg)
    , runTest "LineSmooth" $ do
        let pic = LineSmooth [(0, 0), (50, 100), (100, 0), (150, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll
          svg
          ["<path", "stroke-linecap=\"round\"", "stroke-linejoin=\"round\""]
    , runTest "ThickLineSmooth" $ do
        let pic = ThickLineSmooth [(0, 0), (50, 100), (100, 0)] 8.0
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<path", "stroke-width=\"8"]
    ]


-- | Test Circle primitives
testCircle :: IO [TestResult]
testCircle = do
  putStrLn "\n=== Testing Circle Primitives ==="

  sequence
    [ runTest "Simple circle" $ do
        let pic = Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<circle", "r=\"50\"", "fill=\"none\"", "stroke="]
    , runTest "Circle with different radius" $ do
        let pic = Circle 100
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "r=\"100\""
    , runTest "ThickCircle" $ do
        let pic = ThickCircle 10 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<circle"
    , runTest "ThickCircle zero thickness" $ do
        let pic = ThickCircle 0 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<circle"
    , runTest "Circle with color" $ do
        let pic = Color red $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "stroke=\"rgb(255,0,0)\""
    , runTest "circleSolid" $ do
        let pic = circleSolid 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<circle"
    , runTest "Multiple concentric circles" $ do
        let pic = Pictures [Circle 25, Circle 50, Circle 75, Circle 100]
        let svg = pictureToSVGDoc testSize white pic
        -- Should have multiple circles
        assertBool "Should have multiple circles" (T.count "<circle" svg >= 4)
    ]


-- | Test Arc primitives
testArc :: IO [TestResult]
testArc = do
  putStrLn "\n=== Testing Arc Primitives ==="

  sequence
    [ runTest "Simple arc" $ do
        let pic = Arc 0 90 50
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<path", "d=\"M", "A 50 50"]
    , runTest "Arc 180 degrees" $ do
        let pic = Arc 0 180 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<path"
    , runTest "Arc full circle" $ do
        let pic = Arc 0 360 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<path"
    , runTest "ThickArc" $ do
        let pic = ThickArc 0 90 5 50
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<path", "stroke-width"]
    , runTest "Arc with color" $ do
        let pic = Color cyan $ Arc 45 135 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "stroke=\"rgb(0,255,255)\""
    , runTest "arcSolid" $ do
        let pic = arcSolid 0 90 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<path"
    , runTest "sectorWire" $ do
        let pic = sectorWire 0 90 50
        let svg = pictureToSVGDoc testSize white pic
        -- sectorWire produces arc + two lines
        assertBool
          "Should have arc elements"
          (T.isInfixOf "<path" svg || T.isInfixOf "<polyline" svg)
    ]


-- | Test Text primitives
testText :: IO [TestResult]
testText = do
  putStrLn "\n=== Testing Text Primitives ==="

  sequence
    [ runTest "Simple text" $ do
        let pic = Text "Hello World"
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<text", ">Hello World</text>"]
    , runTest "Text with special characters (XML escape)" $ do
        let pic = Text "A < B & C > D"
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["&lt;", "&amp;", "&gt;"]
    , runTest "Text with quotes" $ do
        let pic = Text "Say \"Hello\""
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "&quot;"
    , runTest "ThickText" $ do
        let pic = ThickText "Bold" 3.0
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<text", "stroke-width"]
    , runTest "Text with color" $ do
        let pic = Color magenta $ Text "Colored"
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "fill=\"rgb(255,0,255)\""
    , runTest "Empty text" $ do
        let pic = Text ""
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<text"
    , runTest "Unicode text" $ do
        let pic = Text "Hello \x4e16\x754c" -- "Hello 世界"
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<text"
    ]


-- | Test Transform primitives
testTransforms :: IO [TestResult]
testTransforms = do
  putStrLn "\n=== Testing Transform Primitives ==="

  sequence
    [ runTest "Translate" $ do
        let pic = Translate 100 50 $ Circle 25
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<g transform=\"translate(", ")\"", "<circle"]
    , runTest "Rotate" $ do
        let pic = Rotate 45 $ rectangleSolid 50 100
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<g transform=\"rotate(", ")\""]
    , runTest "Scale" $ do
        let pic = Scale 2 0.5 $ Circle 25
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<g transform=\"scale(", ")\""]
    , runTest "Uniform scale" $ do
        let pic = Scale 2 2 $ Circle 25
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "scale(2,2)"
    , runTest "Negative scale (mirror)" $ do
        let pic = Scale (-1) 1 $ Circle 25
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "scale(-1,1)"
    , runTest "Combined transforms" $ do
        let pic = Translate 100 100 $ Rotate 45 $ Scale 2 2 $ Circle 25
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["translate(", "rotate(", "scale("]
    , runTest "Nested transforms" $ do
        let pic =
              Translate 50 50 $
                Rotate 30 $
                  Translate 25 0 $
                    Circle 10
        let svg = pictureToSVGDoc testSize white pic
        -- Should have multiple transform groups
        assertBool "Should have nested transforms" (T.count "<g transform" svg >= 2)
    ]


-- | Test Color primitive
testColor :: IO [TestResult]
testColor = do
  putStrLn "\n=== Testing Color Primitive ==="

  sequence
    [ runTest "Color red" $ do
        let pic = Color red $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(255,0,0)"
    , runTest "Color green" $ do
        let pic = Color green $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(0,255,0)"
    , runTest "Color blue" $ do
        let pic = Color blue $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(0,0,255)"
    , runTest "Color yellow" $ do
        let pic = Color yellow $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(255,255,0)"
    , runTest "Color cyan" $ do
        let pic = Color cyan $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(0,255,255)"
    , runTest "Color magenta" $ do
        let pic = Color magenta $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(255,0,255)"
    , runTest "Color with transparency" $ do
        let pic = Color (makeColor 1 0 0 0.5) $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["rgba(255,0,0,0.500)", "opacity"]
    , runTest "makeColorI" $ do
        let pic = Color (makeColorI 128 64 192 255) $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgb(128,64,192)"
    , runTest "Nested colors (inner overrides)" $ do
        let pic = Color red $ Color blue $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        -- The inner color (blue) should be applied to the circle
        assertContains svg "rgb(0,0,255)"
    , runTest "Full transparency" $ do
        let pic = Color (makeColor 1 0 0 0) $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rgba(255,0,0,0.000)"
    ]


-- | Test Pictures (composite) primitive
testPictures :: IO [TestResult]
testPictures = do
  putStrLn "\n=== Testing Pictures Composite ==="

  sequence
    [ runTest "Empty pictures" $ do
        let pic = Pictures []
        let svg = pictureToSVGDoc testSize white pic
        pure $ not (T.isInfixOf "<polygon" svg || T.isInfixOf "<circle" svg)
    , runTest "Single picture" $ do
        let pic = Pictures [Circle 50]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<circle"
    , runTest "Multiple pictures" $ do
        let pic =
              Pictures
                [ Circle 50
                , Translate 100 0 $ Circle 25
                , Polygon [(0, 0), (50, 0), (25, 50)]
                ]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["<circle", "<polygon"]
    , runTest "Nested pictures" $ do
        let pic =
              Pictures
                [ Pictures [Circle 25, Circle 50]
                , Pictures [Circle 75, Circle 100]
                ]
        let svg = pictureToSVGDoc testSize white pic
        assertBool "Should have multiple circles" (T.count "<circle" svg >= 4)
    , runTest "Pictures with different colors" $ do
        let pic =
              Pictures
                [ Color red $ Circle 50
                , Color blue $ Translate 100 0 $ Circle 50
                , Color green $ Translate (-100) 0 $ Circle 50
                ]
        let svg = pictureToSVGDoc testSize white pic
        assertContainsAll svg ["rgb(255,0,0)", "rgb(0,0,255)", "rgb(0,255,0)"]
    , runTest "Large pictures list" $ do
        let pic =
              Pictures [Translate (fromIntegral i * 10) 0 $ Circle 5 | i <- [0 :: Int .. 20]]
        let svg = pictureToSVGDoc testSize white pic
        assertBool "Should have many circles" (T.count "<circle" svg >= 20)
    ]


-- | Test rectangle helper functions
testRectangles :: IO [TestResult]
testRectangles = do
  putStrLn "\n=== Testing Rectangle Helpers ==="

  sequence
    [ runTest "rectangleSolid" $ do
        let pic = rectangleSolid 100 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polygon"
    , runTest "rectangleWire" $ do
        let pic = rectangleWire 100 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polyline"
    , runTest "rectangleUpperSolid" $ do
        let pic = rectangleUpperSolid 100 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polygon"
    , runTest "rectangleUpperWire" $ do
        let pic = rectangleUpperWire 100 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polyline"
    , runTest "lineLoop (closed rectangle)" $ do
        let pic = lineLoop $ rectanglePath 100 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polyline"
    ]


-- | Test line loops
testLineLoop :: IO [TestResult]
testLineLoop = do
  putStrLn "\n=== Testing Line Loops ==="

  sequence
    [ runTest "Simple lineLoop" $ do
        let pic = lineLoop [(0, 0), (100, 0), (100, 100), (0, 100)]
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<polyline"
    , runTest "Empty lineLoop" $ do
        let pic = lineLoop []
        let svg = pictureToSVGDoc testSize white pic
        pure $ not (T.isInfixOf "<polyline" svg) || T.isInfixOf "points=\"\"" svg
    , runTest "Triangle lineLoop" $ do
        let pic = lineLoop [(0, 0), (100, 0), (50, 100)]
        let svg = pictureToSVGDoc testSize white pic
        -- The loop should close back to the first point
        assertContains svg "<polyline"
    ]


-- | Test edge cases and special values
testEdgeCases :: IO [TestResult]
testEdgeCases = do
  putStrLn "\n=== Testing Edge Cases ==="

  sequence
    [ runTest "Zero radius circle" $ do
        let pic = Circle 0
        let svg = pictureToSVGDoc testSize white pic
        -- Should still produce valid SVG
        assertContains svg "r=\"0\""
    , runTest "Negative radius circle" $ do
        let pic = Circle (-50)
        let svg = pictureToSVGDoc testSize white pic
        -- Should use absolute value
        assertContains svg "r=\"50\""
    , runTest "Very small values" $ do
        let pic = Circle 0.001
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "<circle"
    , runTest "Very large values" $ do
        let pic = Circle 10000
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "r=\"10000\""
    , runTest "Zero scale" $ do
        let pic = Scale 0 0 $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "scale(0,0)"
    , runTest "Very small scale" $ do
        let pic = Scale 0.001 0.001 $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "scale("
    , runTest "Large rotation" $ do
        let pic = Rotate 720 $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rotate("
    , runTest "Negative rotation" $ do
        let pic = Rotate (-45) $ Circle 50
        let svg = pictureToSVGDoc testSize white pic
        assertContains svg "rotate("
    ]


-- | Test file export functionality
testFileExport :: IO [TestResult]
testFileExport = do
  putStrLn "\n=== Testing File Export ==="
  outputDir <- getTestOutputDir

  sequence
    [ runTest "Export single picture to file" $ do
        let pic = Pictures [Circle 50, Color red $ rectangleSolid 30 30]
        let filePath = outputDir </> "test_single.svg"
        exportPictureToSVG testSize white filePath pic
        exists <- doesFileExist filePath
        if exists
          then do
            content <- TIO.readFile filePath
            assertContainsAll content ["<?xml", "<svg", "<circle", "<polygon", "</svg>"]
          else pure False
    , runTest "Export animation to files" $ do
        let animation t = Circle (25 + t)
        let filePath = outputDir </> "test_anim%d.svg"
        exportPicturesToSVG testSize white filePath animation [10, 20, 30]
        -- Check that all frames exist
        exists1 <- doesFileExist $ outputDir </> "test_anim1.svg"
        exists2 <- doesFileExist $ outputDir </> "test_anim2.svg"
        exists3 <- doesFileExist $ outputDir </> "test_anim3.svg"
        pure $ exists1 && exists2 && exists3
    , runTest "Export with different sizes" $ do
        let pic = Circle 50
        let filePath1 = outputDir </> "test_size1.svg"
        let filePath2 = outputDir </> "test_size2.svg"
        exportPictureToSVG (100, 100) white filePath1 pic
        exportPictureToSVG (800, 600) white filePath2 pic
        content1 <- TIO.readFile filePath1
        content2 <- TIO.readFile filePath2
        result1 <- assertContains content1 "width=\"100\""
        result2 <- assertContains content2 "width=\"800\""
        pure $ result1 && result2
    ]


-- | Test comprehensive scene
testComprehensiveScene :: IO [TestResult]
testComprehensiveScene = do
  putStrLn "\n=== Testing Comprehensive Scene ==="
  outputDir <- getTestOutputDir

  sequence
    [ runTest "Complex scene with all primitives" $ do
        let scene =
              Pictures
                [ -- Background shapes
                  Color (makeColor 0.9 0.9 1 1) $ rectangleSolid 380 380
                , -- Circles
                  Translate (-100) 100 $ Color red $ circleSolid 40
                , Translate 0 100 $ Color blue $ Circle 40
                , Translate 100 100 $ Color green $ ThickCircle 10 40
                , -- Polygons
                  Translate (-100) 0 $ Color yellow $ Polygon [(0, 0), (40, 0), (20, 40)]
                , Translate 0 0 $ Color cyan $ rectangleSolid 60 40
                , Translate 100 0 $ Color magenta $ rectangleWire 60 40
                , -- Lines
                  Translate (-100) (-100) $ Color orange $ Line [(0, 0), (40, 40)]
                , Translate 0 (-100) $ Color rose $ ThickLine [(0, 0), (40, 40)] 5
                , -- Arcs
                  Translate 100 (-100) $ Color violet $ Arc 0 180 30
                , -- Text
                  Translate 0 (-150) $ Color black $ Scale 0.5 0.5 $ Text "Hello SVG!"
                , -- Transformed shapes
                  Translate 0 150 $
                    Rotate 45 $
                      Scale 0.5 0.5 $
                        Color azure $
                          rectangleSolid 80 80
                ]
        let filePath = outputDir </> "comprehensive_test.svg"
        exportPictureToSVG (400, 400) white filePath scene
        content <- TIO.readFile filePath
        assertContainsAll
          content
          [ "<svg"
          , "<circle"
          , "<polygon"
          , "<polyline"
          , "<path"
          , "<text"
          , "translate("
          , "rotate("
          , "scale("
          , "</svg>"
          ]
    , runTest "Deep nesting" $ do
        let deepNest =
              Translate 0 0 $
                Rotate 15 $
                  Scale 0.9 0.9 $
                    Translate 10 0 $
                      Rotate 15 $
                        Scale 0.9 0.9 $
                          Translate 10 0 $
                            Rotate 15 $
                              Scale 0.9 0.9 $
                                Circle 50
        let svg = pictureToSVGDoc testSize white deepNest
        assertContainsAll svg ["translate(", "rotate(", "scale(", "<circle"]
    ]


-- | Test color variations
testColorVariations :: IO [TestResult]
testColorVariations = do
  putStrLn "\n=== Testing Color Variations ==="

  sequence
    [ runTest "Predefined colors" $ do
        let colors :: [(String, Color)]
            colors =
              [ ("red", red)
              , ("green", green)
              , ("blue", blue)
              , ("yellow", yellow)
              , ("cyan", cyan)
              , ("magenta", magenta)
              , ("black", black)
              , ("white", white)
              , ("orange", orange)
              , ("rose", rose)
              , ("violet", violet)
              , ("azure", azure)
              , ("aquamarine", aquamarine)
              , ("chartreuse", chartreuse)
              ]
        let pics =
              [ Color c $ Translate (fromIntegral i * 50 - 300) 0 $ Circle 20
              | (i, (_, c)) <- zip [0 :: Int ..] colors
              ]
        let svg = pictureToSVGDoc (800, 200) white $ Pictures pics
        assertBool "Should have many circles with colors" (T.count "<circle" svg >= 10)
    , runTest "Gradient-like colors" $ do
        let pics =
              [ Color (makeColor (i / 10) 0 (1 - i / 10) 1) $
                Translate (i * 40 - 200) 0 $
                  Circle 15
              | i <- [0, 1 .. 10]
              ]
        let svg = pictureToSVGDoc (500, 100) white $ Pictures pics
        assertBool "Should have gradient colors" (T.count "<circle" svg >= 10)
    , runTest "Alpha gradient" $ do
        let pics =
              [ Color (makeColor 1 0 0 (i / 10)) $
                Translate (i * 40 - 200) 0 $
                  circleSolid 30
              | i <- [1 .. 10]
              ]
        let svg = pictureToSVGDoc (500, 100) white $ Pictures pics
        assertContains svg "opacity"
    ]


-- | Main test runner
main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  Brillo SVG Export - Comprehensive Tests"
  putStrLn "=========================================="

  -- Run all test categories
  results <-
    concat
      <$> sequence
        [ testDocumentStructure
        , testBlank
        , testPolygon
        , testLine
        , testCircle
        , testArc
        , testText
        , testTransforms
        , testColor
        , testPictures
        , testRectangles
        , testLineLoop
        , testEdgeCases
        , testFileExport
        , testComprehensiveScene
        , testColorVariations
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
    then do
      putStrLn "\nAll tests passed!"
      exitSuccess
    else do
      putStrLn $ "\n" ++ show (total - passes) ++ " test(s) failed."
      exitFailure
