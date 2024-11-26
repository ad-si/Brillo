{-# LANGUAGE PatternGuards #-}

-- | Advance the world to the next time step.
module Advance where

import Actor (Actor (..), Force, Index, Time)
import Collide (collideBeadBeadElastic, collideBeadBeadStatic, collideBeadWall)
import Config (beadStuckCount, gravityCoeff)
import Contact (findContacts)
import World (World (..))

import Brillo.Data.Point.Arithmetic qualified as Pt
import Brillo.Data.Vector (magV, mulSV, rotateV)
import Brillo.Geometry.Angle (degToRad)
import Brillo.Interface.Pure.Simulate (ViewPort (viewPortRotate))

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set


-- Advance ---------------------------------------------------------------------

-- | Advance all the actors in this world by a certain time.
advanceWorld
  :: ViewPort
  -- ^ current viewport
  -> Time
  -- ^ time to advance them for.
  -> World
  -- ^ the world to advance.
  -> World
  -- ^ the new world.
advanceWorld viewport time (World actors tree) =
  let
    rot = viewPortRotate viewport
    force = rotateV (degToRad rot) (0, negate gravityCoeff)

    -- move all the actors
    actors_moved = Map.map (moveActorFree time force) actors

    -- find contacts in the world
    (contacts, tree') =
      findContacts (World actors_moved tree)

    -- apply contacts to each pair of actors
    actors_bounced =
      Set.fold
        (applyContact time force)
        actors_moved
        contacts
  in
    World actors_bounced tree'


-- Move two actors which are known to be in contact.
applyContact
  :: Time
  -- ^ time step
  -> Force
  -- ^ ambient force on the actors
  -> (Index, Index)
  -- ^ indicies of the the two actors in contact
  -> Map Index Actor
  -- ^ the old world
  -> Map Index Actor
  -- ^ the new world
applyContact _time _force (ix1, ix2) actors =
  -- use the indicies to lookup the data for each actor from the map
  case (Map.lookup ix1 actors, Map.lookup ix2 actors) of
    (Just a1, Just a2) ->
      let
        resultActors
          -- handle a collision between bead and a wall
          | Bead _ _ _r1 _p1 _v1 <- a1
          , Wall{} <- a2 =
              let a1' = collideBeadWall a1 a2
              in  Map.insert ix1 a1' actors
          -- handle a collision between two beads
          | Bead _ix1 m1 _r1 _p1 _v1 <- a1
          , Bead _ix2 m2 _r2 _p2 _v2 <- a2 =
              let
                (a1', a2')
                  -- if one of the beads is stuck then do a safer, static collision.
                  -- with this method the beads don't transfer energy into each other
                  -- so there is less of a chance of lots of beads being crushed
                  -- together if there are many in the same place.
                  | m1 >= beadStuckCount || m2 >= beadStuckCount =
                      let a1'' = collideBeadBeadStatic a1 a2
                          a2'' = collideBeadBeadStatic a2 a1
                      in  (a1'', a2'')
                  -- otherwise do the real elastic collision
                  --      this is much more realistic.
                  | otherwise =
                      collideBeadBeadElastic a1 a2
              in
                -- write the new data for the actors back into the map
                Map.insert ix1 a1' $
                  Map.insert ix2 a2' actors
          | otherwise =
              actors
      in
        resultActors
    _ -> actors


-- | Move a bead which isn't in contact with anything else.
moveActorFree
  :: Time
  -- ^ time to move it for
  -> Force
  -- ^ ambient force on the actor during this time
  -> Actor
  -- ^ the bead to move
  -> Actor
  -- ^ the new bead
moveActorFree time force actor
  -- move a bead
  | Bead ix stuck radius pos vel <- actor =
      let
        -- assume all beads have the same mass.
        beadMass = 1

        -- calculate the new position and velocity of the bead.
        pos' = (pos Pt.+ time `mulSV` vel)
        vel' = (vel Pt.+ (time / beadMass) `mulSV` force)

        -- if the bead is travelling slowly then set it as being stuck.
        stuck'
          | magV vel' < 20 =
              min beadStuckCount (stuck + 1)
          | otherwise =
              max 0 (stuck - 2)
      in
        Bead ix stuck' radius pos' vel'
  -- walls don't move
  | Wall{} <- actor =
      actor
