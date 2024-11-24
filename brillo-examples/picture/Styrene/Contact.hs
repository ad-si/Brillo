{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- | Find actors in the world that are in contact with each other.
module Contact where

import Actor (Actor (..), Index, actorIx, isBead, isWall)
import Brillo.Data.Point (Point)
import Brillo.Geometry.Line (
  closestPointOnLine,
  closestPointOnLineParam,
 )
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (
  Float (F#),
  Float#,
  gtFloat#,
  ltFloat#,
  minusFloat#,
  plusFloat#,
  sqrtFloat#,
  tagToEnum#,
  timesFloat#,
 )
import QuadTree (QuadTree, treeElems, treeZero)
import World (World (..), insertActor)


-- Find all pairs of actors in the world that are in contact with each other.
findContacts
  :: World
  -> ( -- a set of all pairs of actors that are in contact.
       Set (Index, Index)
     , -- also return the quadtree so we can draw it in the window.
       QuadTree Actor
     )
findContacts (World actors _) =
  let
    -- the initial tree has no actors in it and has a
    --      size of 300 (with is half the width of the box).
    treeInit = treeZero 300

    -- insert all the actors into the quadtree.
    tree' = Map.foldr' insertActor treeInit actors

    -- the potential contacts are lists of actors
    --      that _might_ be in contact.
    potentialContacts =
      treeElems tree'

    -- filter the lists of potential contacts to determine the actors
    --      which are _actually_ in contact.
    contactSet = makeContacts potentialContacts
  in
    (contactSet, tree')


{-| Make add all these test pairs to a map
     normalise so the actor with the lowest ix is first in the pair.
-}
makeContacts :: [[Actor]] -> Set (Index, Index)
makeContacts contactLists =
  makeContacts' Set.empty contactLists


makeContacts' :: Set (Index, Index) -> [[Actor]] -> Set (Index, Index)
makeContacts' acc xx =
  case xx of
    -- no more potentials to add, return the current contact set
    [] -> acc
    -- add pairs of actors that are actually in contact to the contact set
    (list : lists) ->
      makeContacts' (makeTests acc list) lists


makeTests :: Set (Index, Index) -> [Actor] -> Set (Index, Index)
makeTests acc [] = acc
makeTests acc (x : xs) =
  makeTests (makeTests1 acc x xs) xs


makeTests1 :: Set (Index, Index) -> Actor -> [Actor] -> Set (Index, Index)
makeTests1 acc _a1 [] = acc
makeTests1 acc a1 (a2 : as)
  | inContact a1 a2 =
      let k1 = actorIx a1
          k2 = actorIx a2
          contact = (min k1 k2, max k1 k2)
          acc' = Set.insert contact acc
      in  makeTests1 acc' a1 as
  | otherwise =
      makeTests1 acc a1 as


-- See if these two actors are in contact
inContact :: Actor -> Actor -> Bool
inContact a1 a2
  | isBead a1 && isWall a2 = inContactBeadWall a1 a2
  | isWall a1 && isBead a2 = inContactBeadWall a2 a1
  | isBead a1 && isBead a2 = inContactBeadBead a1 a2
  | otherwise = False


-- | Check whether a bead is in contact with a wall.
inContactBeadWall :: Actor -> Actor -> Bool
inContactBeadWall
  (Bead _ix _mode radius pBead _)
  (Wall _ pWall1 pWall2) =
    let
      -- work out the point on the infinite line between pWall1 and pWall2
      --      which is closest to the bead.
      pClosest = closestPointOnLine pWall1 pWall2 pBead

      -- the distance between the bead center and pClosest
      --      needs to be less than the bead radius for them to touch.
      !(F# radius#) = radius
      closeEnough = distancePPContact pBead pClosest `ltFloat#` radius#

      -- uParam gives where pClosest is relative to the endponts of the wall
      uParam = closestPointOnLineParam pWall1 pWall2 pBead

      -- pClosest needs to lie on the line segment between pWal1 and pWall2
      inSegment = uParam >= 0 && uParam <= 1
    in
      tagToEnum# closeEnough && inSegment
inContactBeadWall _ _ = False


-- | Check whether a bead is in concat with another bead.
inContactBeadBead :: Actor -> Actor -> Bool
inContactBeadBead
  (Bead _ix1 _ radius1 pBead1 _)
  (Bead _ix2 _ radius2 pBead2 _) =
    let !dist# = distancePPContact pBead1 pBead2
        !(F# rad) = radius1 + radius2
    in  tagToEnum# (dist# `ltFloat#` rad) && tagToEnum# (dist# `gtFloat#` 0.1#)
inContactBeadBead _ _ = False


-- | Return the distance between these two points.
{-# INLINE distancePPContact #-}
distancePPContact :: Point -> Point -> Float#
distancePPContact (F# x1, F# y1) (F# x2, F# y2) = do
  let
    !xd = x2 `minusFloat#` x1
    !xd2 = xd `timesFloat#` xd

    !yd = y2 `minusFloat#` y1
    !yd2 = yd `timesFloat#` yd

  sqrtFloat# (xd2 `plusFloat#` yd2)
