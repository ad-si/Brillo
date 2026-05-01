{-# LANGUAGE OverloadedStrings #-}

-- | Example of custom GLSL shaders rendered via the Brillo 'Shader' picture type.
module Main where

import Brillo
import Data.Text (Text)


main :: IO ()
main =
  animate
    ( InWindow
        "Custom Shader" -- window title
        (600, 600) -- window size
        (10, 10) -- window position
    )
    black
    frame


frame :: Float -> Picture
frame t =
  Pictures
    [ -- Rounded rectangle on the left
      Translate (-160) 0 $
        Color (makeColorI 255 200 0 255) $
          shader
            200
            200
            roundedRectFrag
            [ ("uSize", UniformVec2 200 200)
            , ("uRadius", UniformFloat 30)
            ]
    , -- Animated flame on the right
      Translate 160 (-20) $
        shader
          200
          300
          flameFrag
          [("uTime", UniformFloat t)]
    ]


-- | Fragment shader that draws a rounded rectangle using a signed distance field.
roundedRectFrag :: Text
roundedRectFrag =
  "#version 120\n\
  \varying vec2 vLocalCoord;\n\
  \uniform vec2 uSize;\n\
  \uniform float uRadius;\n\
  \void main() {\n\
  \  vec2 halfSize = uSize * 0.5 - vec2(uRadius);\n\
  \  vec2 d = abs(vLocalCoord) - halfSize;\n\
  \  float dist = length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - uRadius;\n\
  \  float pixelSize = length(vec2(dFdx(dist), dFdy(dist)));\n\
  \  float alpha = 1.0 - smoothstep(-pixelSize, pixelSize, dist);\n\
  \  if (alpha < 0.001) discard;\n\
  \  gl_FragColor = vec4(gl_Color.rgb, gl_Color.a * alpha);\n\
  \}"


{-| Fragment shader that draws a simple procedural flame.
Uses layered sine waves distorted over time to shape the flame,
colored from deep red at the tip through orange to bright yellow at the base.
-}
flameFrag :: Text
flameFrag =
  "#version 120\n\
  \varying vec2 vLocalCoord;\n\
  \uniform float uTime;\n\
  \void main() {\n\
  \  vec2 uv = vLocalCoord;\n\
  \  float h = 150.0;\n\
  \  float ny = (uv.y + h) / (2.0 * h);\n\
  \  float nx = uv.x / 100.0;\n\
  \  float envelope = (1.0 - ny) * (1.0 - ny);\n\
  \  float wave = 0.0;\n\
  \  wave += 0.3 * sin(nx * 6.0 + uTime * 3.0 + ny * 4.0);\n\
  \  wave += 0.2 * sin(nx * 10.0 - uTime * 5.0 + ny * 6.0);\n\
  \  wave += 0.1 * sin(nx * 16.0 + uTime * 7.0);\n\
  \  float shape = envelope - abs(nx + wave * 0.3 * ny);\n\
  \  shape = smoothstep(0.0, 0.5, shape);\n\
  \  if (shape < 0.001) discard;\n\
  \  float t = clamp(ny, 0.0, 1.0);\n\
  \  // Deep red at tip, orange in middle, bright yellow at base\n\
  \  vec3 tipColor = vec3(0.6, 0.0, 0.0);\n\
  \  vec3 midColor = vec3(0.9, 0.15, 0.0);\n\
  \  vec3 baseColor = vec3(1.0, 0.7, 0.1);\n\
  \  vec3 col = mix(baseColor, midColor, t);\n\
  \  col = mix(col, tipColor, t * t);\n\
  \  // Slight bright core at bottom center\n\
  \  float core = (1.0 - t) * (1.0 - abs(nx) * 2.0);\n\
  \  core = max(core, 0.0);\n\
  \  col = mix(col, vec3(1.0, 0.85, 0.4), core * core);\n\
  \  gl_FragColor = vec4(col, shape);\n\
  \}"
