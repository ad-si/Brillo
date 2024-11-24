module Brillo.Data.Quad (
  Quad (..),
  allQuads,
)
where


-- | Represents a Quadrant in the 2D plane.
data Quad
  = -- | North West
    NW
  | -- | North East
    NE
  | -- | South West
    SW
  | -- | South East
    SE
  deriving (Show, Eq, Enum)


-- | A list of all quadrants. Same as @[NW .. SE]@.
allQuads :: [Quad]
allQuads = [NW .. SE]
