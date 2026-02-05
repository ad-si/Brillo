{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Fast(ish) rendering of circles.
module Brillo.Internals.Rendering.Circle (
  renderCircle,
  renderCircleSmooth,
  renderArc,
  renderArcSmooth,
)
where

import Brillo.Internals.Rendering.Common (gf)
import GHC.Exts (
  Float (F#),
  Float#,
  cosFloat#,
  divideFloat#,
  geFloat#,
  plusFloat#,
  sinFloat#,
  timesFloat#,
 )
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL


-- | Tau = 2 * pi, the ratio of circumference to radius
tau :: Float
tau = 2 * pi
{-# INLINE tau #-}


-------------------------------------------------------------------------------

{-| Decide how many line segments to use to render the circle.
  The number of segments we should use to get a nice picture depends on
  the size of the circle on the screen, not its intrinsic radius.
  If the viewport has been zoomed-in then we need to use more segments.
-}
circleSteps :: Float -> Int
circleSteps sDiam
  | sDiam < 8 = 8
  | sDiam < 16 = 16
  | sDiam < 32 = 32
  | otherwise = 64
{-# INLINE circleSteps #-}


-- Circle ---------------------------------------------------------------------

-- | Render a circle with the given thickness
renderCircle :: Float -> Float -> Float -> Float -> Float -> IO ()
renderCircle posX posY scaleFactor radius_ thickness_ =
  go (abs radius_) (abs thickness_)
  where
    go radius thickness
      -- If the circle is smaller than a pixel, render it as a point.
      | thickness == 0
      , radScreen <- scaleFactor * (radius + thickness / 2)
      , radScreen <= 1 =
          GL.renderPrimitive GL.Points $
            GL.vertex $
              GL.Vertex2 (gf posX) (gf posY)
      -- Render zero thickness circles with lines.
      | thickness == 0
      , radScreen <- scaleFactor * radius
      , steps <- circleSteps radScreen =
          renderCircleLine posX posY steps radius
      -- Some thick circle.
      | radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen =
          renderCircleStrip posX posY steps radius thickness


-- | Render a circle as a line.
renderCircleLine :: Float -> Float -> Int -> Float -> IO ()
renderCircleLine (F# posX) (F# posY) steps (F# rad) =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStop) = tau
  in  GL.renderPrimitive GL.LineLoop $
        renderCircleLineStep posX posY tStep tStop rad 0.0#
{-# INLINE renderCircleLine #-}


-- | Render a circle with a given thickness as a triangle strip
renderCircleStrip :: Float -> Float -> Int -> Float -> Float -> IO ()
renderCircleStrip (F# posX) (F# posY) steps r width =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStop) = tau + F# tStep / 2
      !(F# r1) = r - width / 2
      !(F# r2) = r + width / 2
  in  GL.renderPrimitive GL.TriangleStrip $
        renderCircleStripStep
          posX
          posY
          tStep
          tStop
          r1
          0.0#
          r2
          (tStep `divideFloat#` 2.0#)
{-# INLINE renderCircleStrip #-}


-- | Render a ring with given inner and outer radii as a triangle strip
renderCircleStripRadii :: Float -> Float -> Int -> Float -> Float -> IO ()
renderCircleStripRadii (F# posX) (F# posY) steps innerR outerR =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStop) = tau + F# tStep / 2
      !(F# r1) = innerR
      !(F# r2) = outerR
  in  GL.renderPrimitive GL.TriangleStrip $
        renderCircleStripStep
          posX
          posY
          tStep
          tStop
          r1
          0.0#
          r2
          (tStep `divideFloat#` 2.0#)
{-# INLINE renderCircleStripRadii #-}


{-| Render a circle with the given thickness, with anti-aliasing.
  Draws filled shape slightly inset, then anti-aliased edge on boundary.
-}
renderCircleSmooth :: Float -> Float -> Float -> Float -> Float -> IO ()
renderCircleSmooth posX posY scaleFactor radius_ thickness_ =
  go (abs radius_) (abs thickness_)
  where
    go radius thickness
      -- If the circle is smaller than a pixel, render it as a point.
      | thickness == 0
      , radScreen <- scaleFactor * (radius + thickness / 2)
      , radScreen <= 1 =
          GL.renderPrimitive GL.Points $
            GL.vertex $
              GL.Vertex2 (gf posX) (gf posY)
      -- Render zero thickness circles with smoothed lines.
      | thickness == 0
      , radScreen <- scaleFactor * radius
      , steps <- circleSteps radScreen =
          do
            GL.lineSmooth $= GL.Enabled
            renderCircleLine posX posY steps radius
            GL.lineSmooth $= GL.Disabled
      -- Solid circle (no inner hole): draw fill then smooth edge on top
      | radius - thickness / 2 <= 0
      , radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen * 2 -- More segments for smoother result
        =
          do
            let outerRadius = radius + thickness / 2
            -- Draw fill at full size
            renderCircleFan posX posY steps outerRadius
            -- Draw anti-aliased edge at exact boundary
            GL.lineSmooth $= GL.Enabled
            renderCircleLine posX posY steps outerRadius
            GL.lineSmooth $= GL.Disabled
      -- Thick circle with inner hole: draw fill then smooth edges on top
      | radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen * 2 -- More segments for smoother result
        =
          do
            let outerRadius = radius + thickness / 2
            let innerRadius = radius - thickness / 2
            -- Draw fill at full size
            renderCircleStripRadii posX posY steps innerRadius outerRadius
            -- Draw anti-aliased edges at exact boundaries
            GL.lineSmooth $= GL.Enabled
            renderCircleLine posX posY steps outerRadius
            renderCircleLine posX posY steps innerRadius
            GL.lineSmooth $= GL.Disabled


-- | Render a filled circle as a triangle fan
renderCircleFan :: Float -> Float -> Int -> Float -> IO ()
renderCircleFan (F# posX) (F# posY) steps (F# rad) =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStop) = tau
  in  GL.renderPrimitive GL.TriangleFan $ do
        -- Center vertex
        GL.vertex $ GL.Vertex2 (gf (F# posX)) (gf (F# posY))
        -- Edge vertices
        renderCircleLineStep posX posY tStep tStop rad 0.0#
        -- Close the fan by repeating the first edge vertex
        addPointOnCircle posX posY rad 0.0#
{-# INLINE renderCircleFan #-}


-- Arc ------------------------------------------------------------------------

-- | Render an arc with the given thickness.
renderArc ::
  Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderArc posX posY scaleFactor radius_ a1 a2 thickness_ =
  go (abs radius_) (abs thickness_)
  where
    go radius thickness
      -- Render zero thickness arcs with lines.
      | thickness == 0
      , radScreen <- scaleFactor * radius
      , steps <- circleSteps radScreen =
          renderArcLine posX posY steps radius a1 a2
      -- Some thick arc.
      | radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen =
          renderArcStrip posX posY steps radius a1 a2 thickness


{-| Render an arc with the given thickness, with anti-aliasing.
  Draws filled shape slightly inset, then anti-aliased edge on boundary.
-}
renderArcSmooth ::
  Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderArcSmooth posX posY scaleFactor radius_ a1 a2 thickness_ =
  go (abs radius_) (abs thickness_)
  where
    go radius thickness
      -- Render zero thickness arcs with smoothed lines.
      | thickness == 0
      , radScreen <- scaleFactor * radius
      , steps <- circleSteps radScreen =
          do
            GL.lineSmooth $= GL.Enabled
            renderArcLine posX posY steps radius a1 a2
            GL.lineSmooth $= GL.Disabled
      -- Solid arc (no inner hole): draw fill then smooth edge on top
      | radius - thickness / 2 <= 0
      , radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen * 2 -- More segments for smoother result
        =
          do
            let outerRadius = radius + thickness / 2
            -- Draw fill at full size
            renderArcFan posX posY steps outerRadius a1 a2
            -- Draw anti-aliased edge at exact boundary
            GL.lineSmooth $= GL.Enabled
            renderArcLine posX posY steps outerRadius a1 a2
            GL.lineSmooth $= GL.Disabled
      -- Thick arc with inner hole: draw fill then smooth edges on top
      | radScreen <- scaleFactor * (radius + thickness / 2)
      , steps <- circleSteps radScreen * 2 -- More segments for smoother result
        =
          do
            let outerRadius = radius + thickness / 2
            let innerRadius = radius - thickness / 2
            -- Draw fill at full size
            renderArcStripRadii posX posY steps innerRadius outerRadius a1 a2
            -- Draw anti-aliased edges at exact boundaries
            GL.lineSmooth $= GL.Enabled
            renderArcLine posX posY steps outerRadius a1 a2
            renderArcLine posX posY steps innerRadius a1 a2
            GL.lineSmooth $= GL.Disabled


-- | Render an arc as a line.
renderArcLine ::
  Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderArcLine (F# posX) (F# posY) steps (F# rad) a1 a2 =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStart) = degToRad a1
      !(F# tStop) = degToRad a2 + if a1 >= a2 then tau else 0

      -- force the line to end at the desired angle
      endVertex = addPointOnCircle posX posY rad tStop
  in  GL.renderPrimitive GL.LineStrip $
        do
          renderCircleLineStep posX posY tStep tStop rad tStart
          endVertex
{-# INLINE renderArcLine #-}


-- | Render a filled arc (pie slice) as a triangle fan from center
renderArcFan :: Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderArcFan (F# posX) (F# posY) steps (F# rad) a1 a2 =
  let n = fromIntegral steps
      !(F# tStep) = tau / n
      !(F# tStart) = degToRad a1
      !(F# tStop) = degToRad a2 + if a1 >= a2 then tau else 0
  in  GL.renderPrimitive GL.TriangleFan $ do
        -- Center vertex
        GL.vertex $ GL.Vertex2 (gf (F# posX)) (gf (F# posY))
        -- Edge vertices along the arc
        renderCircleLineStep posX posY tStep tStop rad tStart
        -- Final vertex at exact end angle
        addPointOnCircle posX posY rad tStop
{-# INLINE renderArcFan #-}


-- | Render an arc with a given thickness as a triangle strip
renderArcStrip ::
  Float -> Float -> Int -> Float -> Float -> Float -> Float -> IO ()
renderArcStrip (F# posX) (F# posY) steps r a1 a2 width =
  let n = fromIntegral steps
      tStep = tau / n

      tStart = degToRad a1
      tStop = degToRad a2 + if a1 >= a2 then tau else 0
      tDiff = tStop - tStart
      tMid = tStart + tDiff / 2

      !(F# tStep') = tStep
      !(F# tStep2') = tStep / 2
      !(F# tStart') = tStart
      !(F# tStop') = tStop
      !(F# tCut') = tStop - tStep
      !(F# tMid') = tMid
      !(F# r1') = r - width / 2
      !(F# r2') = r + width / 2
  in  GL.renderPrimitive GL.TriangleStrip $
        do
          -- start vector
          addPointOnCircle posX posY r1' tStart'
          addPointOnCircle posX posY r2' tStart'

          -- If we don't have a complete step then just drop a point
          -- between the two ending lines.
          if tDiff < tStep
            then do
              addPointOnCircle posX posY r1' tMid'

              -- end vectors
              addPointOnCircle posX posY r2' tStop'
              addPointOnCircle posX posY r1' tStop'
            else do
              renderCircleStripStep
                posX
                posY
                tStep'
                tCut'
                r1'
                tStart'
                r2'
                (tStart' `plusFloat#` tStep2')

              -- end vectors
              addPointOnCircle posX posY r1' tStop'
              addPointOnCircle posX posY r2' tStop'
{-# INLINE renderArcStrip #-}


-- | Render an arc with given inner and outer radii as a triangle strip
renderArcStripRadii ::
  Float -> Float -> Int -> Float -> Float -> Float -> Float -> IO ()
renderArcStripRadii (F# posX) (F# posY) steps innerR outerR a1 a2 =
  let n = fromIntegral steps
      tStep = tau / n

      tStart = degToRad a1
      tStop = degToRad a2 + if a1 >= a2 then tau else 0
      tDiff = tStop - tStart
      tMid = tStart + tDiff / 2

      !(F# tStep') = tStep
      !(F# tStep2') = tStep / 2
      !(F# tStart') = tStart
      !(F# tStop') = tStop
      !(F# tCut') = tStop - tStep
      !(F# tMid') = tMid
      !(F# r1') = innerR
      !(F# r2') = outerR
  in  GL.renderPrimitive GL.TriangleStrip $
        do
          -- start vector
          addPointOnCircle posX posY r1' tStart'
          addPointOnCircle posX posY r2' tStart'

          -- If we don't have a complete step then just drop a point
          -- between the two ending lines.
          if tDiff < tStep
            then do
              addPointOnCircle posX posY r1' tMid'

              -- end vectors
              addPointOnCircle posX posY r2' tStop'
              addPointOnCircle posX posY r1' tStop'
            else do
              renderCircleStripStep
                posX
                posY
                tStep'
                tCut'
                r1'
                tStart'
                r2'
                (tStart' `plusFloat#` tStep2')

              -- end vectors
              addPointOnCircle posX posY r1' tStop'
              addPointOnCircle posX posY r2' tStop'
{-# INLINE renderArcStripRadii #-}


-- Step functions -------------------------------------------------------------
renderCircleLineStep ::
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  IO ()
renderCircleLineStep posX posY tStep tStop rad tt
  | 1# <- tt `geFloat#` tStop =
      return ()
  | otherwise =
      do
        addPointOnCircle posX posY rad tt
        renderCircleLineStep
          posX
          posY
          tStep
          tStop
          rad
          (tt `plusFloat#` tStep)
{-# INLINE renderCircleLineStep #-}


renderCircleStripStep ::
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  Float# ->
  IO ()
renderCircleStripStep posX posY tStep tStop r1 t1 r2 t2
  | 1# <- t1 `geFloat#` tStop =
      return ()
  | otherwise =
      do
        addPointOnCircle posX posY r1 t1
        addPointOnCircle posX posY r2 t2
        renderCircleStripStep
          posX
          posY
          tStep
          tStop
          r1
          (t1 `plusFloat#` tStep)
          r2
          (t2 `plusFloat#` tStep)
{-# INLINE renderCircleStripStep #-}


addPoint :: Float# -> Float# -> IO ()
addPoint x y =
  GL.vertex $ GL.Vertex2 (gf (F# x)) (gf (F# y))
{-# INLINE addPoint #-}


addPointOnCircle :: Float# -> Float# -> Float# -> Float# -> IO ()
addPointOnCircle posX posY rad tt =
  addPoint
    (posX `plusFloat#` (rad `timesFloat#` cosFloat# tt))
    (posY `plusFloat#` (rad `timesFloat#` sinFloat# tt))
{-# INLINE addPointOnCircle #-}


-- | Convert degrees to radians
degToRad :: Float -> Float
degToRad d = d * pi / 180
{-# INLINE degToRad #-}

{- Unused sector drawing code.
   Sectors are currently drawn as compound Pictures,
   but we might want this if we end up implementing the ThickSector
   version as well.

-- | Render a sector as a line.
renderSectorLine :: Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderSectorLine pX@(F# posX) pY@(F# posY) steps (F# rad) a1 a2
 = let  n               = fromIntegral steps
        !(F# tStep)     = tau / n
        !(F# tStart)    = degToRad a1
        !(F# tStop)     = degToRad a2 + if a1 >= a2 then tau else 0

        -- need to set up the edges of the start/end triangles
        startVertex     = GL.vertex $ GL.Vertex2 (gf pX) (gf pY)
        endVertex       = addPointOnCircle posX posY rad tStop

   in   GL.renderPrimitive GL.LineLoop
         $ do   startVertex
                renderCircleLineStep posX posY tStep tStop rad tStart
                endVertex

-- | Render a sector.
renderSector :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderSector posX posY scaleFactor radius a1 a2
        | radScreen     <- scaleFactor * radius
        , steps         <- circleSteps (2 * radScreen)
        = renderSectorLine posX posY steps radius a1 a2
-}
