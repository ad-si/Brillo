module Util (sumPair) where

  sumPair :: (Num a) => (a, a) -> (a, a) -> (a, a)
  sumPair (x1, y1) (x2, y2) = (x1+x2, y1+y2)