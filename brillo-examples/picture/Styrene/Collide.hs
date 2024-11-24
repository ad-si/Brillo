-- | Physics for bead bouncing.
module Collide where

import Actor (Actor (..))
import Brillo.Data.Point (Point)
import Brillo.Data.Point.Arithmetic qualified as Pt
import Brillo.Data.Vector (
  angleVV,
  detV,
  dotV,
  mulSV,
  normalizeV,
  rotateV,
 )
import Brillo.Geometry.Line (closestPointOnLine)


-- Config -----------------------------------------------------------------------------------------
-- How bouncy the beads are
--      at 0.2 and they look like melting plastic.
--      at 0.8 and they look like bouncy rubber balls.
--      at > 1 and they gain energy with each bounce and escape the box.
--
beadBeadLoss :: Float
beadBeadLoss = 0.95
beadWallLoss :: Float
beadWallLoss = 0.8


-- | Move a bead which is in contact with a wall.
collideBeadWall
  :: Actor
  -- ^ the bead
  -> Actor
  -- ^ the wall that bead is in contact with
  -> Actor
  -- ^ the new bead
collideBeadWall
  bead@(Bead _ix _ _radius pBead _vIn)
  (Wall _ pWall1 pWall2) =
    let
      -- Take the collision point as being the point on the wall which is
      -- closest to the bead's center.
      pCollision = closestPointOnLine pWall1 pWall2 pBead
    in
      -- then do a static, non energy transfering collision.
      collideBeadPointStatic
        bead
        pCollision
        beadWallLoss
collideBeadWall _ _ = error "collideBeadWall: not a bead and a wall"


-- | Move two beads which have bounced into each other.
collideBeadBeadElastic
  :: Actor
  -> Actor
  -> (Actor, Actor)
collideBeadBeadElastic
  (Bead ix1 mode1 r1 p1 v1)
  (Bead ix2 mode2 r2 p2 v2) =
    let mass1 = 1
        mass2 = 1

        -- the axis of collision (towards p2)
        vCollision@(cX, cY) = normalizeV (p2 Pt.- p1)
        vCollisionR = (cY, -cX)

        -- the velocity component of each bead along the axis of collision
        s1 = dotV v1 vCollision
        s2 = dotV v2 vCollision

        -- work out new velocities along the collision
        s1' = (s1 * (mass1 - mass2) + 2 * mass2 * s2) / (mass1 + mass2)
        s2' = (s2 * (mass2 - mass1) + 2 * mass1 * s1) / (mass1 + mass2)

        -- the velocity components at right angles to the collision
        --      there is no friction in the collision so these don't change
        k1 = dotV v1 vCollisionR
        k2 = dotV v2 vCollisionR

        -- new bead velocities
        v1' = mulSV s1' vCollision Pt.+ mulSV k1 vCollisionR
        v2' = mulSV s2' vCollision Pt.+ mulSV k2 vCollisionR

        v1_slow = mulSV beadBeadLoss v1'
        v2_slow = mulSV beadBeadLoss v2'

        -- work out the point of collision
        u1 = r1 / (r1 + r2)

        pCollision =
          p1 Pt.+ mulSV u1 (p2 Pt.- p1)

        -- place the beads just next to each other so they are no longer overlapping.
        p1' = pCollision Pt.- (r1 + 0.001) `mulSV` vCollision
        p2' = pCollision Pt.+ (r2 + 0.001) `mulSV` vCollision

        bead1' = Bead ix1 mode1 r1 p1' v1_slow
        bead2' = Bead ix2 mode2 r2 p2' v2_slow
    in  (bead1', bead2')
collideBeadBeadElastic _ _ = error "collideBeadBeadElastic: not two beads"


collideBeadBeadStatic
  :: Actor
  -> Actor
  -> Actor
collideBeadBeadStatic
  bead1@(Bead _ix1 _ radius1 pBead1 _)
  (Bead _ix2 _ radius2 pBead2 _) =
    let
      -- Take the collision point as being between the center's of the two beads.
      -- For beads which have the same radius the collision point is half way between
      -- their centers and u == 0.5
      u = radius1 / (radius1 + radius2)
      pCollision = pBead1 Pt.+ mulSV u (pBead2 Pt.- pBead1)

      bead1' =
        collideBeadPointStatic
          bead1
          pCollision
          beadBeadLoss
    in
      bead1'
collideBeadBeadStatic _ _ = error "collideBeadBeadStatic: not two beads"


-- | Move a bead which has collided with something.
collideBeadPointStatic
  :: Actor
  -- ^ the bead which collided with something
  -> Point
  -- ^ the point of collision (should be near the bead's surface)
  -> Float
  -- ^ velocity scaling factor (how much to slow the bead down after the collision)
  -> Actor
collideBeadPointStatic
  (Bead ix mode radius pBead vIn)
  pCollision
  velLoss =
    let
      -- take a normal vector from the wall to the bead.
      --      this vector is at a right angle to the wall.
      vNormal = normalizeV (pBead Pt.- pCollision)

      -- the bead at pBead is overlapping with what it collided with, but we don't want that.
      --      place the bead so it's surface is just next to the point of collision.
      pBead_new = pCollision Pt.+ (radius + 0.01) `mulSV` vNormal

      -- work out the angle of incidence for the bounce.
      --      this is the angle between the surface normal and
      --      the direction of travel for the bead.
      aInc = angleVV vNormal (Pt.negate vIn)

      -- aInc2 is the angle between the wall /surface/ and
      --      the direction of travel.
      aInc2 = (pi / 2) - aInc

      -- take the determinant between the surface normal and the direction of travel.
      --      This will tell us what direction the bead hit the wall.
      --      The diagram shows the sign of the determinant for the four possiblities.
      --
      --           \ +ve                                -ve /
      --            \                                      /
      --             \/                                  \/
      --   pWall1 ---------- pWall2           pWall1 ---------- pWall2
      --             /\                                  /\
      --            /                                      \
      --           / -ve                                +ve \
      --
      determinant = detV vIn vNormal

      -- Use the determinant to rotate the bead's velocity vector for the bounce.
      vOut
        | determinant > 0 = rotateV (2 * aInc2) vIn
        | otherwise = rotateV (negate (2 * aInc2)) vIn

      -- Slow down the bead when it hits the wall
      vSlow = velLoss `mulSV` vOut

      bead1_new = Bead ix mode radius pBead_new vSlow
    in
      bead1_new
collideBeadPointStatic _ _ _ =
  error "collideBeadPointStatic: not a bead and a point"
