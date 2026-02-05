{-# OPTIONS_HADDOCK hide #-}

-- | Shader-based rendering for anti-aliased shapes using signed distance fields.
module Brillo.Internals.Rendering.Shader (
  ShaderState (..),
  initShaderState,
  renderCircleSDF,
  renderArcSDF,
  renderThickLineSDF,
) where

import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL


-- | State for shader-based rendering
data ShaderState = ShaderState
  { circleProgram :: !(IORef (Maybe GL.Program))
  , arcProgram :: !(IORef (Maybe GL.Program))
  , lineProgram :: !(IORef (Maybe GL.Program))
  }


-- | Initialize shader state (programs are compiled lazily on first use)
initShaderState :: IO ShaderState
initShaderState = do
  circleRef <- newIORef Nothing
  arcRef <- newIORef Nothing
  lineRef <- newIORef Nothing
  return
    ShaderState
      { circleProgram = circleRef
      , arcProgram = arcRef
      , lineProgram = lineRef
      }


-- | Vertex shader for SDF shapes - uses texture coords for local position
vertexShaderSrc :: String
vertexShaderSrc =
  unlines
    [ "#version 120"
    , "varying vec2 vLocalCoord;"
    , "void main() {"
    , "  vLocalCoord = gl_MultiTexCoord0.xy;"
    , "  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;"
    , "}"
    ]


-- | Fragment shader for filled circle with SDF anti-aliasing
circleFragmentShaderSrc :: String
circleFragmentShaderSrc =
  unlines
    [ "#version 120"
    , "varying vec2 vLocalCoord;"
    , "uniform vec4 uColor;"
    , "uniform float uOuterRadius;"
    , "uniform float uInnerRadius;" -- 0 for solid circle
    , "uniform float uPixelSize;" -- Size of a pixel in local coords for AA width
    , ""
    , "void main() {"
    , "  float dist = length(vLocalCoord);"
    , "  // Clamp AA width to at most 5% of outer radius to avoid fuzziness when zoomed out"
    , "  float aaWidth = min(uPixelSize, uOuterRadius * 0.05);"
    , ""
    , "  // Outer edge"
    , "  float outerAlpha = 1.0 - smoothstep(uOuterRadius - aaWidth, uOuterRadius + aaWidth, dist);"
    , ""
    , "  // Inner edge (for rings) - only apply if inner radius > 0"
    , "  float innerAlpha = uInnerRadius > 0.0 ? smoothstep(uInnerRadius - aaWidth, uInnerRadius + aaWidth, dist) : 1.0;"
    , ""
    , "  float alpha = outerAlpha * innerAlpha;"
    , "  if (alpha < 0.001) discard;"
    , "  gl_FragColor = vec4(uColor.rgb, uColor.a * alpha);"
    , "}"
    ]


{-| Fragment shader for arc with SDF anti-aliasing
Arc is drawn counter-clockwise from start angle to end angle
-}
arcFragmentShaderSrc :: String
arcFragmentShaderSrc =
  unlines
    [ "#version 120"
    , "varying vec2 vLocalCoord;"
    , "uniform vec4 uColor;"
    , "uniform float uOuterRadius;"
    , "uniform float uInnerRadius;"
    , "uniform float uStartAngle;" -- In radians
    , "uniform float uSweep;" -- Arc sweep in radians (always positive, counter-clockwise)
    , "uniform float uPixelSize;"
    , ""
    , "#define PI 3.14159265359"
    , "#define TAU 6.28318530718"
    , ""
    , "void main() {"
    , "  float dist = length(vLocalCoord);"
    , "  float angle = atan(vLocalCoord.y, vLocalCoord.x);"
    , "  // Clamp AA width to at most 5% of outer radius to avoid fuzziness when zoomed out"
    , "  float aaWidth = min(uPixelSize, uOuterRadius * 0.05);"
    , ""
    , "  // Outer edge"
    , "  float outerAlpha = 1.0 - smoothstep(uOuterRadius - aaWidth, uOuterRadius + aaWidth, dist);"
    , ""
    , "  // Inner edge (only apply if inner radius > 0)"
    , "  float innerAlpha = uInnerRadius > 0.0 ? smoothstep(uInnerRadius - aaWidth, uInnerRadius + aaWidth, dist) : 1.0;"
    , ""
    , "  // Calculate angle relative to start, normalized to [0, 2*PI)"
    , "  float relAngle = angle - uStartAngle;"
    , "  // Normalize to [0, 2*PI) - add TAU twice to handle very negative values"
    , "  relAngle = relAngle - TAU * floor(relAngle / TAU);"
    , ""
    , "  // Soft edges at arc boundaries using signed distance from boundaries"
    , "  float angularAA = aaWidth / max(dist, 0.001);"
    , ""
    , "  // Distance from start edge (positive = inside arc)"
    , "  float startDist = relAngle;"
    , "  // Distance from end edge (positive = inside arc)"
    , "  float endDist = uSweep - relAngle;"
    , ""
    , "  float startAlpha = smoothstep(-angularAA, angularAA, startDist);"
    , "  float endAlpha = smoothstep(-angularAA, angularAA, endDist);"
    , ""
    , "  float alpha = outerAlpha * innerAlpha * startAlpha * endAlpha;"
    , "  if (alpha < 0.001) discard;"
    , "  gl_FragColor = vec4(uColor.rgb, uColor.a * alpha);"
    , "}"
    ]


{-| Fragment shader for line segment (capsule) with SDF anti-aliasing.
A thick line segment is rendered as a capsule - all points within
a given distance from the line segment.
-}
lineFragmentShaderSrc :: String
lineFragmentShaderSrc =
  "#version 120 \n\
  \varying vec2 vLocalCoord; \
  \uniform vec4 uColor; \
  \uniform vec2 uPointA; \
  \uniform vec2 uPointB; \
  \uniform float uThickness; \
  \uniform float uPixelSize; \
  \void main() { \
  \  vec2 pa = vLocalCoord - uPointA; \
  \  vec2 ba = uPointB - uPointA; \
  \  float len2 = dot(ba, ba); \
  \  float h = len2 > 0.0 ? clamp(dot(pa, ba) / len2, 0.0, 1.0) : 0.0; \
  \  float dist = length(pa - ba * h); \
  \  float aaWidth = min(uPixelSize, uThickness * 0.1); \
  \  float d = dist - uThickness; \
  \  float alpha = 1.0 - smoothstep(-aaWidth, aaWidth, d); \
  \  if (alpha < 0.001) discard; \
  \  gl_FragColor = vec4(uColor.rgb, uColor.a * alpha); \
  \}"


-- | Compile a shader from source
compileShader :: GL.ShaderType -> String -> IO GL.Shader
compileShader shaderType src = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= GL.packUtf8 src
  GL.compileShader shader
  ok <- GL.get (GL.compileStatus shader)
  unless ok $ do
    infoLog <- GL.get (GL.shaderInfoLog shader)
    error $ "Shader compilation failed: " ++ infoLog
  return shader


-- | Create and link a shader program
createProgram :: String -> String -> IO GL.Program
createProgram vertSrc fragSrc = do
  vertShader <- compileShader GL.VertexShader vertSrc
  fragShader <- compileShader GL.FragmentShader fragSrc

  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program fragShader

  GL.linkProgram program
  ok <- GL.get (GL.linkStatus program)
  unless ok $ do
    infoLog <- GL.get (GL.programInfoLog program)
    error $ "Program linking failed: " ++ infoLog

  -- Clean up shaders (they're copied into the program)
  GL.deleteObjectName vertShader
  GL.deleteObjectName fragShader

  return program


-- | Get or create the circle shader program
getCircleProgram :: ShaderState -> IO GL.Program
getCircleProgram state = do
  maybeProgram <- readIORef (circleProgram state)
  case maybeProgram of
    Just prog -> return prog
    Nothing -> do
      prog <- createProgram vertexShaderSrc circleFragmentShaderSrc
      writeIORef (circleProgram state) (Just prog)
      return prog


-- | Get or create the arc shader program
getArcProgram :: ShaderState -> IO GL.Program
getArcProgram state = do
  maybeProgram <- readIORef (arcProgram state)
  case maybeProgram of
    Just prog -> return prog
    Nothing -> do
      prog <- createProgram vertexShaderSrc arcFragmentShaderSrc
      writeIORef (arcProgram state) (Just prog)
      return prog


-- | Get or create the line shader program
getLineProgram :: ShaderState -> IO GL.Program
getLineProgram state = do
  maybeProgram <- readIORef (lineProgram state)
  case maybeProgram of
    Just prog -> return prog
    Nothing -> do
      prog <- createProgram vertexShaderSrc lineFragmentShaderSrc
      writeIORef (lineProgram state) (Just prog)
      return prog


-- | Render a circle/ring using SDF shader
renderCircleSDF ::
  ShaderState ->
  Float -> -- posX
  Float -> -- posY
  Float -> -- scaleFactor (pixels per unit)
  Float -> -- outer radius
  Float -> -- inner radius (0 for solid)
  GL.Color4 GL.GLfloat -> -- color
  IO ()
renderCircleSDF state posX posY scaleFactor outerR innerR color = do
  program <- getCircleProgram state

  -- Save current program
  oldProgram <- GL.get GL.currentProgram

  GL.currentProgram $= Just program

  -- Set uniforms
  uColorLoc <- GL.get (GL.uniformLocation program "uColor")
  uOuterLoc <- GL.get (GL.uniformLocation program "uOuterRadius")
  uInnerLoc <- GL.get (GL.uniformLocation program "uInnerRadius")
  uPixelLoc <- GL.get (GL.uniformLocation program "uPixelSize")

  GL.uniform uColorLoc $= color
  GL.uniform uOuterLoc $= (realToFrac outerR :: GL.GLfloat)
  GL.uniform uInnerLoc $= (realToFrac innerR :: GL.GLfloat)
  GL.uniform uPixelLoc $= (realToFrac (1.0 / scaleFactor) :: GL.GLfloat)

  -- Draw a quad that covers the circle
  let r = outerR + 2.0 / scaleFactor -- Add padding for AA
      x1 = posX - r
      x2 = posX + r
      y1 = posY - r
      y2 = posY + r
      -- Local coords relative to circle center
      l1 = -r
      l2 = r

  -- Draw quad with position (vertex) and local coords (texCoord)
  GL.renderPrimitive GL.Quads $ do
    GL.texCoord $
      GL.TexCoord2 (realToFrac l1 :: GL.GLfloat) (realToFrac l1 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x1 :: GL.GLfloat) (realToFrac y1 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l2 :: GL.GLfloat) (realToFrac l1 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x2 :: GL.GLfloat) (realToFrac y1 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l2 :: GL.GLfloat) (realToFrac l2 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x2 :: GL.GLfloat) (realToFrac y2 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l1 :: GL.GLfloat) (realToFrac l2 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x1 :: GL.GLfloat) (realToFrac y2 :: GL.GLfloat)

  GL.currentProgram $= oldProgram


-- | Render an arc using SDF shader
renderArcSDF ::
  ShaderState ->
  Float -> -- posX
  Float -> -- posY
  Float -> -- scaleFactor
  Float -> -- outer radius
  Float -> -- inner radius
  Float -> -- start angle (degrees)
  Float -> -- end angle (degrees)
  GL.Color4 GL.GLfloat -> -- color
  IO ()
renderArcSDF state posX posY scaleFactor outerR innerR startDeg endDeg color = do
  program <- getArcProgram state

  oldProgram <- GL.get GL.currentProgram
  GL.currentProgram $= Just program

  -- Set uniforms
  uColorLoc <- GL.get (GL.uniformLocation program "uColor")
  uOuterLoc <- GL.get (GL.uniformLocation program "uOuterRadius")
  uInnerLoc <- GL.get (GL.uniformLocation program "uInnerRadius")
  uStartLoc <- GL.get (GL.uniformLocation program "uStartAngle")
  uSweepLoc <- GL.get (GL.uniformLocation program "uSweep")
  uPixelLoc <- GL.get (GL.uniformLocation program "uPixelSize")

  -- Convert to radians and calculate sweep (counter-clockwise from start to end)
  let startRad = startDeg * pi / 180.0
      endRad = endDeg * pi / 180.0
      -- Sweep is how far we go counter-clockwise from start to end
      -- If start >= end, we wrap around through 360 degrees
      sweep =
        if startDeg >= endDeg
          then (endRad + 2 * pi) - startRad
          else endRad - startRad

  GL.uniform uColorLoc $= color
  GL.uniform uOuterLoc $= (realToFrac outerR :: GL.GLfloat)
  GL.uniform uInnerLoc $= (realToFrac innerR :: GL.GLfloat)
  GL.uniform uStartLoc $= (realToFrac startRad :: GL.GLfloat)
  GL.uniform uSweepLoc $= (realToFrac sweep :: GL.GLfloat)
  GL.uniform uPixelLoc $= (realToFrac (1.0 / scaleFactor) :: GL.GLfloat)

  -- Draw a quad that covers the arc
  let r = outerR + 2.0 / scaleFactor
      x1 = posX - r
      x2 = posX + r
      y1 = posY - r
      y2 = posY + r
      l1 = -r
      l2 = r

  GL.renderPrimitive GL.Quads $ do
    GL.texCoord $
      GL.TexCoord2 (realToFrac l1 :: GL.GLfloat) (realToFrac l1 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x1 :: GL.GLfloat) (realToFrac y1 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l2 :: GL.GLfloat) (realToFrac l1 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x2 :: GL.GLfloat) (realToFrac y1 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l2 :: GL.GLfloat) (realToFrac l2 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x2 :: GL.GLfloat) (realToFrac y2 :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac l1 :: GL.GLfloat) (realToFrac l2 :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac x1 :: GL.GLfloat) (realToFrac y2 :: GL.GLfloat)

  GL.currentProgram $= oldProgram


-- | Render a single line segment using SDF shader (capsule shape)
renderLineSegmentSDF ::
  ShaderState ->
  Float -> -- ax
  Float -> -- ay
  Float -> -- bx
  Float -> -- by
  Float -> -- scaleFactor (pixels per unit)
  Float -> -- thickness (diameter)
  GL.Color4 GL.GLfloat -> -- color
  IO ()
renderLineSegmentSDF state ax ay bx by scaleFactor thickness color = do
  program <- getLineProgram state

  oldProgram <- GL.get GL.currentProgram
  GL.currentProgram $= Just program

  -- Set uniforms
  uColorLoc <- GL.get (GL.uniformLocation program "uColor")
  uPointALoc <- GL.get (GL.uniformLocation program "uPointA")
  uPointBLoc <- GL.get (GL.uniformLocation program "uPointB")
  uThicknessLoc <- GL.get (GL.uniformLocation program "uThickness")
  uPixelLoc <- GL.get (GL.uniformLocation program "uPixelSize")

  let halfThick = thickness / 2.0

  GL.uniform uColorLoc $= color
  GL.uniform uPointALoc
    $= GL.Vector2 (realToFrac ax :: GL.GLfloat) (realToFrac ay)
  GL.uniform uPointBLoc
    $= GL.Vector2 (realToFrac bx :: GL.GLfloat) (realToFrac by)
  GL.uniform uThicknessLoc $= (realToFrac halfThick :: GL.GLfloat)
  GL.uniform uPixelLoc $= (realToFrac (1.0 / scaleFactor) :: GL.GLfloat)

  -- Compute bounding box with padding for AA
  let padding = 2.0 / scaleFactor
      r = halfThick + padding
      minX = min ax bx - r
      maxX = max ax bx + r
      minY = min ay by - r
      maxY = max ay by + r

  -- Draw quad covering the capsule
  -- vLocalCoord receives the vertex position (world space)
  GL.renderPrimitive GL.Quads $ do
    GL.texCoord $
      GL.TexCoord2 (realToFrac minX :: GL.GLfloat) (realToFrac minY :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac minX :: GL.GLfloat) (realToFrac minY :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac maxX :: GL.GLfloat) (realToFrac minY :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac maxX :: GL.GLfloat) (realToFrac minY :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac maxX :: GL.GLfloat) (realToFrac maxY :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac maxX :: GL.GLfloat) (realToFrac maxY :: GL.GLfloat)

    GL.texCoord $
      GL.TexCoord2 (realToFrac minX :: GL.GLfloat) (realToFrac maxY :: GL.GLfloat)
    GL.vertex $
      GL.Vertex2 (realToFrac minX :: GL.GLfloat) (realToFrac maxY :: GL.GLfloat)

  GL.currentProgram $= oldProgram


-- | Render an entire path as connected thick line segments using SDF
renderThickLineSDF ::
  ShaderState ->
  [(Float, Float)] -> -- Path points
  Float -> -- scaleFactor
  Float -> -- thickness
  GL.Color4 GL.GLfloat -> -- color
  IO ()
renderThickLineSDF _state [] _ _ _ = return ()
renderThickLineSDF _state [_] _ _ _ = return () -- Single point, nothing to draw
renderThickLineSDF state path scaleFactor thickness color = do
  -- Render each segment as a capsule
  -- Overlapping capsules at joins naturally create rounded joins
  let segments = zip path (drop 1 path)
  mapM_ renderSegment segments
  where
    renderSegment ((ax, ay), (bx, by)) =
      renderLineSegmentSDF state ax ay bx by scaleFactor thickness color
