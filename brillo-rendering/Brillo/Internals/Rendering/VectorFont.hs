-- | Inspired by https://hackage.haskell.org/package/VectorFont
module Brillo.Internals.Rendering.VectorFont (
  VectorFont,
  VFWidth,
  VFPoint,
  VFStroke,
  VFGlyph,
  render,
  renderSafe,
  optimizeStrokes,
  canvastextFont,
) where

import Control.Monad (replicateM, (>=>))
import Data.Either (partitionEithers)
import Data.List (minimumBy, permutations)
import Data.Map (Map, findWithDefault, fromList, lookup)
import Data.Ord (comparing)


type VFWidth = Double
type VFPoint = (Double, Double)


-- | @VFStroke@ is a series of points joined by straight lines.
type VFStroke = [VFPoint]


-- | @VFGlyph@ is the symbol's width, plus the strokes needed to draw it
type VFGlyph = (VFWidth, [VFStroke])


dist :: VFPoint -> VFPoint -> Double
dist (xa, ya) (xb, yb) =
  sqrt $ (xa - xb) ^ (2 :: Int) + (ya - yb) ^ (2 :: Int)


-- | @VectorFont@ is a map from @Char@ to @VFGlyph@
newtype VectorFont = VectorFont (Map Char VFGlyph)


{-| Given a @VectorFont@ and a @String@, return
  * @Right@ strokes if the @String@ can be rendered.
  * @Left@ error otherwise.
-}
render :: VectorFont -> String -> Either String [VFStroke]
render f =
  getGlyphs f >=> (Right . renderLine 0)


getGlyphs :: VectorFont -> String -> Either String [VFGlyph]
getGlyphs f =
  addErrorMsg . leftsOrRights . map (getGlyph f)


addErrorMsg :: Either String a -> Either String a
addErrorMsg (Left a) = Left $ "Missing chars: " ++ a
addErrorMsg a = a


-- | Like @T.sequence@ but gives all the @Left@s if any exist
leftsOrRights :: [Either a b] -> Either [a] [b]
leftsOrRights = do
  let
    pick ([], bs) = Right bs
    pick (as, _) = Left as
  pick . partitionEithers


getGlyph :: VectorFont -> Char -> Either Char VFGlyph
getGlyph (VectorFont m) c =
  maybe (Left c) Right $ c `Data.Map.lookup` m


{-| Like @render@, but replaces unknown characters with a question mark.

It is guaranteed to render something, and thus
useful if you want to ignore the possibility of errors e.g.
because you're manually checking the output.
-}
renderSafe :: VectorFont -> String -> [VFStroke]
renderSafe f =
  renderLine 0 . map (getGlyphSafe f)


getGlyphSafe :: VectorFont -> Char -> VFGlyph
getGlyphSafe (VectorFont m) c =
  findWithDefault fallbackGlyph c m


{-| This is the `?` from @canvastextFont@,
duplicated here to avoid constraining @canvastextFont@ to contain `?`
-}
fallbackGlyph :: VFGlyph
fallbackGlyph =
  ( 18
  , optimizeStrokes
      [
        [ (3, 16)
        , (3, 17)
        , (4, 19)
        , (5, 20)
        , (7, 21)
        , (11, 21)
        , (13, 20)
        , (14, 19)
        , (15, 17)
        , (15, 15)
        , (14, 13)
        , (13, 12)
        , (9, 10)
        , (9, 7)
        ]
      , [(9, 2), (8, 1), (9, 0), (10, 1), (9, 2)]
      ]
  )


renderLine :: VFWidth -> [VFGlyph] -> [VFStroke]
renderLine _ [] = []
renderLine dx ((w, ss) : sss) = do
  let offset = (fmap . fmap) (\(x, y) -> (x + dx, y))
  offset ss ++ renderLine (dx + w) sss


{-| Given a set of strokes, try to optimize their order and direction:
* Fewer strokes
* Smaller gaps between strokes
* Left-most starting position
* Bottom-most starting position

This improves the fonts where no thought has been given to this.
However, hand-tweaking is still better.
-}
optimizeStrokes :: [VFStroke] -> [VFStroke]
optimizeStrokes = do
  let pickBest = minimumBy (comparing score)
  pickBest . map joinStrokes . allArrangements . filter (not . null)


allArrangements :: [[a]] -> [[[a]]]
allArrangements =
  concatMap allDirs . permutations


allDirs :: [[a]] -> [[[a]]]
allDirs strokes = do
  let opss = replicateM (length strokes) [id, reverse]
  [zipWith ($) ops strokes | ops <- opss]


-- given an arrangement of strokes, return a score (lower better),
-- preferring (in order):
score :: [VFStroke] -> (Int, Double, VFPoint)
score ss | length ss < 2 = (length ss, 0.0, (0.0, 0.0))
score ss = do
  let
    skips = zipWith (\as bs -> dist (last as) (head bs)) ss (tail ss)
    firstPoint = head $ head ss
  (length ss, sum skips, firstPoint)


joinStrokes :: (Eq a) => [[a]] -> [[a]]
joinStrokes (s0 : s1 : ss)
  | last s0 == head s1 = joinStrokes $ (s0 ++ tail s1) : ss
  | otherwise = s0 : joinStrokes (s1 : ss)
joinStrokes ss = ss

{- FOURMOLU_DISABLE -}
-- | Based on Hershey font and includes following characters:
-- @!\"#$%&()*+,-.\/0123456789:;\<=>?\@ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- []^_\`abcdefghijklmnopqrstuvwxyz{|}~@
canvastextFont :: VectorFont
canvastextFont = VectorFont $ fromList
  [ (' ',(12,[]))
  , ('!',(6,[[(5,2),(4,1),(5,0),(6,1),(5,2)],[(5,7),(5,21)]]))
  , ('"',(6,[[(4,15),(5,16),(6,18),(6,20),(5,21),(4,20),(5,19)]]))
  , ('#',(17,[[(10,-7),(17,25)],[(11,25),(4,-7)]
      ,[(3,6),(17,6)],[(18,12),(4,12)]]))
  , ('$',(16,[[(3,3),(5,1),(8,0),(12,0),(15,1),(17,3),(17,6),(16,8),(15,9)
      ,(13,10),(7,12),(5,13),(4,14),(3,16),(3,18),(5,20),(8,21),(12,21)
      ,(15,20),(17,18)],[(12,25),(12,-4)],[(8,-4),(8,25)]]))
  , ('%',(20,[[(8,21),(10,19),(10,17),(9,15),(7,14),(5,14),(3,16),(3,18)
      ,(4,20),(6,21),(8,21),(10,20),(13,19),(16,19),(19,20),(21,21),(3,0)]
      ,[(17,7),(15,6),(14,4),(14,2),(16,0)
      ,(18,0),(20,1),(21,3),(21,5),(19,7),(17,7)]]))
  , ('&',(22,[[(23,2),(23,1),(22,0),(20,0),(18,1),(16,3),(11,10),(9,13)
      ,(8,16),(8,18),(9,20),(11,21),(13,20),(14,18),(14,16),(13,14),(12,13)
      ,(5,9),(4,8),(3,6),(3,4),(4,2),(5,1),(7,0),(11,0),(13,1),(15,3),(17,6)
      ,(19,11),(20,13),(21,14),(22,14),(23,13),(23,12)]]))
  , ('(',(10,[[(11,-7),(9,-5),(7,-2),(5,2)
      ,(4,7),(4,11),(5,16),(7,20),(9,23),(11,25)]]))
  , (')',(10,[[(3,-7),(5,-5),(7,-2),(9,2)
      ,(10,7),(10,11),(9,16),(7,20),(5,23),(3,25)]]))
  , ('*',(12,[[(3,12),(13,18)],[(8,21),(8,9)],[(13,12),(3,18)]]))
  , ('+',(22,[[(4,9),(22,9)],[(13,0),(13,18)]]))
  , (',',(6,[[(4,-4),(5,-3),(6,-1),(6,1),(5,2),(4,1),(5,0),(6,1)]]))
  , ('-',(22,[[(4,9),(22,9)]]))
  , ('.',(6,[[(5,2),(4,1),(5,0),(6,1),(5,2)]]))
  , ('/',(18,[[(2,-7),(20,25)]]))
  , ('0',(16,[[(9,21),(6,20),(4,17),(3,12),(3,9),(4,4),(6,1),(9,0),(11,0)
      ,(14,1),(16,4),(17,9),(17,12),(16,17),(14,20),(11,21),(9,21)]]))
  , ('1',(16,[[(6,17),(8,18),(11,21),(11,0)]]))
  , ('2',(16,[[(4,16),(4,17),(5,19),(6,20),(8,21),(12,21),(14,20),(15,19)
      ,(16,17),(16,15),(15,13),(13,10),(3,0),(17,0)]]))
  , ('3',(16,[[(3,4),(4,2),(5,1),(8,0),(11,0),(14,1),(16,3),(17,6),(17,8)
      ,(16,11),(15,12),(13,13),(10,13),(16,21),(5,21)]]))
  , ('4',(16,[[(13,0),(13,21),(3,7),(18,7)]]))
  , ('5',(16,[[(3,4),(4,2),(5,1),(8,0),(11,0),(14,1),(16,3),(17,6),(17,8)
      ,(16,11),(14,13),(11,14),(8,14),(5,13),(4,12),(5,21),(15,21)]]))
  , ('6',(16,[[(4,7),(5,10),(7,12),(10,13),(11,13),(14,12),(16,10),(17,7)
      ,(17,6),(16,3),(14,1),(11,0),(10,0),(7,1),(5,3)
      ,(4,7),(4,12),(5,17),(7,20),(10,21),(12,21),(15,20),(16,18)]]))
  , ('7',(16,[[(3,21),(17,21),(7,0)]]))
  , ('8',(16,[[(8,21),(5,20),(4,18),(4,16),(5,14),(7,13),(11,12),(14,11),(16,9)
      ,(17,7),(17,4),(16,2),(15,1),(12,0),(8,0),(5,1),(4,2),(3,4),(3,7),(4,9)
      ,(6,11),(9,12),(13,13),(15,14),(16,16),(16,18),(15,20),(12,21),(8,21)]]))
  , ('9',(16,[[(4,3),(5,1),(8,0),(10,0),(13,1),(15,4),(16,9),(16,14),(15,18)
      ,(13,20),(10,21),(9,21),(6,20),(4,18),(3,15),(3,14),(4,11),(6,9),(9,8)
      ,(10,8),(13,9),(15,11),(16,14)]]))
  , (':',(6,[[(5,2),(4,1),(5,0),(6,1),(5,2)]
      ,[(5,14),(4,13),(5,12),(6,13),(5,14)]]))
  , (';',(6,[[(4,-4),(5,-3),(6,-1),(6,1),(5,2),(4,1),(5,0),(6,1)]
      ,[(5,14),(4,13),(5,12),(6,13),(5,14)]]))
  , ('<',(20,[[(20,0),(4,9),(20,18)]]))
  , ('=',(22,[[(4,6),(22,6)],[(22,12),(4,12)]]))
  , ('>',(20,[[(4,0),(20,9),(4,18)]]))
  , ('?',(14,[[(3,16),(3,17),(4,19),(5,20),(7,21),(11,21),(13,20),(14,19)
      ,(15,17),(15,15),(14,13),(13,12),(9,10),(9,7)]
      ,[(9,2),(8,1),(9,0),(10,1),(9,2)]]))
  , ('@',(23,[[(11,5),(10,6),(9,8),(9,11),(10,14),(12,16)],[(18,13),(17,15)
      ,(15,16),(12,16),(10,15),(9,14),(8,11),(8,8),(9,6),(11,5),(14,5),(16,6)
      ,(17,8)],[(19,5),(18,6),(18,8),(19,16)],[(18,16),(17,8),(17,6),(19,5)
      ,(21,5),(23,7),(24,10),(24,12),(23,15),(22,17),(20,19),(18,20),(15,21)
      ,(12,21),(9,20),(7,19),(5,17),(4,15),(3,12),(3,9),(4,6),(5,4),(7,2)
      ,(9,1),(12,0),(15,0),(18,1),(20,2),(21,3)]]))
  , ('A',(14,[[(1,0),(9,21),(17,0)],[(14,7),(4,7)]]))
  , ('B',(17,[[(4,11),(13,11),(16,10),(17,9),(18,7),(18,4),(17,2),(16,1)
      ,(13,0),(4,0),(4,21),(13,21),(16,20),(17,19)
      ,(18,17),(18,15),(17,13),(16,12),(13,11)]]))
  , ('C',(17,[[(18,5),(17,3),(15,1),(13,0),(9,0),(7,1),(5,3),(4,5),(3,8),(3,13)
      ,(4,16),(5,18),(7,20),(9,21),(13,21),(15,20),(17,18),(18,16)]]))
  , ('D',(17,[[(4,0),(4,21),(11,21),(14,20),(16,18),(17,16),(18,13),(18,8)
      ,(17,5),(16,3),(14,1),(11,0),(4,0)]]))
  , ('E',(15,[[(4,11),(12,11)],[(17,21),(4,21),(4,0),(17,0)]]))
  , ('F',(14,[[(12,11),(4,11)],[(4,0),(4,21),(17,21)]]))
  , ('G',(17,[[(13,8),(18,8),(18,5),(17,3),(15,1),(13,0),(9,0),(7,1),(5,3)
      ,(4,5),(3,8),(3,13),(4,16),(5,18),(7,20)
      ,(9,21),(13,21),(15,20),(17,18),(18,16)]]))
  , ('H',(16,[[(4,0),(4,21)],[(4,11),(16,11)],[(16,21),(16,0)]]))
  , ('I',(6,[[(4,0),(4,21)]]))
  , ('J',(12,[[(2,7),(2,5),(3,2),(4,1)
      ,(6,0),(8,0),(10,1),(11,2),(12,5),(12,21)]]))
  , ('K',(17,[[(18,0),(9,12)],[(4,21),(4,0)],[(4,7),(18,21)]]))
  , ('L',(13,[[(4,21),(4,0),(16,0)]]))
  , ('M',(20,[[(4,0),(4,21),(12,0),(20,21),(20,0)]]))
  , ('N',(18,[[(4,0),(4,21),(18,0),(18,21)]]))
  , ('O',(18,[[(9,21),(7,20),(5,18),(4,16),(3,13),(3,8),(4,5),(5,3),(7,1),(9,0)
      ,(13,0),(15,1),(17,3),(18,5),(19,8)
      ,(19,13),(18,16),(17,18),(15,20),(13,21),(9,21)]]))
  , ('P',(17,[[(4,0),(4,21),(13,21),(16,20),(17,19)
      ,(18,17),(18,14),(17,12),(16,11),(13,10),(4,10)]]))
  , ('Q',(18,[[(9,21),(7,20),(5,18),(4,16),(3,13),(3,8),(4,5),(5,3),(7,1),(9,0)
      ,(13,0),(15,1),(17,3),(18,5),(19,8),(19,13),(18,16)
      ,(17,18),(15,20),(13,21),(9,21)],[(12,4),(18,-2)]]))
  , ('R',(17,[[(4,0),(4,21),(13,21),(16,20),(17,19),(18,17),(18,15),(17,13)
      ,(16,12),(13,11),(4,11)],[(11,11),(18,0)]]))
  , ('S',(16,[[(3,3),(5,1),(8,0),(12,0),(15,1),(17,3),(17,6),(16,8),(15,9)
      ,(13,10),(7,12),(5,13),(4,14),(3,16),(3,18)
      ,(5,20),(8,21),(12,21),(15,20),(17,18)]]))
  , ('T',(12,[[(1,21),(15,21)],[(8,21),(8,0)]]))
  , ('U',(18,[[(4,21),(4,6),(5,3),(7,1)
      ,(10,0),(12,0),(15,1),(17,3),(18,6),(18,21)]]))
  , ('V',(14,[[(1,21),(9,0),(17,21)]]))
  , ('W',(20,[[(2,21),(7,0),(12,15),(17,0),(22,21)]]))
  , ('X',(16,[[(3,0),(17,21)],[(3,21),(17,0)]]))
  , ('Y',(14,[[(1,21),(9,11),(9,0)],[(9,11),(17,21)]]))
  , ('Z',(16,[[(3,21),(17,21),(3,0),(17,0)]]))
  , ('[',(10,[[(5,-7),(5,25)],[(11,25),(4,25),(4,-7),(11,-7)]]))
  , (']',(10,[[(3,-7),(10,-7),(10,25),(3,25)],[(9,25),(9,-7)]]))
  , ('^',(12,[[(2,12),(8,18),(14,12)],[(11,15),(8,19),(5,15)]]))
  , ('_',(12,[[(0,-2),(16,-2)]]))
  , ('`',(6,[[(5,17),(6,16),(5,15),(4,16),(4,18),(5,20),(6,21)]]))
  , ('a',(15,[[(15,0),(15,14)],[(15,11),(13,13),(11,14),(8,14),(6,13),(4,11)
      ,(3,8),(3,6),(4,3),(6,1),(8,0),(11,0),(13,1),(15,3)]]))
  , ('b',(15,[[(4,11),(6,13),(8,14),(11,14),(13,13),(15,11),(16,8),(16,6),(15,3)
      ,(13,1),(11,0),(8,0),(6,1),(4,3)],[(4,0),(4,21)]]))
  , ('c',(15,[[(15,3),(13,1),(11,0),(8,0),(6,1),(4,3),(3,6),(3,8),(4,11),(6,13)
      ,(8,14),(11,14),(13,13),(15,11)]]))
  , ('d',(14,[[(12,11),(10,13),(8,14),(6,14),(4,13),(2,11),(1,8),(1,6),(2,3)
      ,(4,1),(6,0),(8,0),(10,1),(12,3)],[(12,0),(12,21)]]))
  , ('e',(14,[[(3,8),(14,8),(14,10),(13,12),(12,13),(10,14),(8,14),(6,13),(4,11)
      ,(3,8),(3,6),(4,3),(6,1),(8,0),(10,0),(12,1),(14,3)]]))
  , ('f',(8,[[(2,14),(9,14)],[(10,21),(8,21),(6,20),(5,17),(5,0)]]))
  , ('g',(15,[[(6,-6),(8,-7),(11,-7),(13,-6),(14,-5),(15,-2),(15,14)],[(15,11)
      ,(13,13),(11,14),(8,14),(6,13),(4,11),(3,8)
      ,(3,6),(4,3),(6,1),(8,0),(11,0),(13,1),(15,3)]]))
  , ('h',(15,[[(4,21),(4,0)],[(4,10),(7,13)
      ,(9,14),(12,14),(14,13),(15,10),(15,0)]]))
  , ('i',(6,[[(3,21),(4,20),(5,21),(4,22),(3,21)],[(4,14),(4,0)]]))
  , ('j',(6,[[(1,-7),(3,-7),(5,-6),(6,-3),(6,14)]
      ,[(5,21),(6,20),(7,21),(6,22),(5,21)]]))
  , ('k',(13,[[(4,21),(4,0)],[(4,4),(14,14)],[(8,8),(15,0)]]))
  , ('l',(6,[[(3,0),(3,21)]]))
  , ('m',(26,[[(4,0),(4,14)]
      ,[(4,10),(7,13),(9,14),(12,14),(14,13),(15,10),(15,0)]
      ,[(15,10),(18,13),(20,14),(23,14),(25,13),(26,10),(26,0)]]))
  , ('n',(15,[[(4,0),(4,14)]
      ,[(4,10),(7,13),(9,14),(12,14),(14,13),(15,10),(15,0)]]))
  , ('o',(14,[[(11,3),(10,1),(8,0),(6,0),(4,1),(3,3),(2,5),(2,7),(2,9),(3,11)
      ,(4,13),(6,14),(8,14),(10,13),(11,11),(12,9),(12,7),(12,5),(11,3)]]))
  , ('p',(15,[[(4,-7),(4,14)],[(4,11),(6,13),(8,14),(11,14),(13,13),(15,11)
      ,(16,8),(16,6),(15,3),(13,1),(11,0),(8,0),(6,1),(4,3)]]))
  , ('q',(15,[[(15,-7),(15,14)],[(15,11),(13,13),(11,14),(8,14),(6,13),(4,11)
      ,(3,8),(3,6),(4,3),(6,1),(8,0),(11,0),(13,1),(15,3)]]))
  , ('r',(9,[[(3,0),(3,14)],[(3,8),(4,11),(6,13),(8,14),(10,14)]]))
  , ('s',(13,[[(3,3),(4,1),(7,0),(10,0),(13,1),(14,3),(14,4),(13,6),(11,7),(6,8)
      ,(4,9),(3,11),(4,13),(7,14),(10,14),(13,13),(14,11)]]))
  , ('t',(8,[[(9,14),(2,14)],[(5,21),(5,4),(6,1),(8,0),(10,0)]]))
  , ('u',(15,[[(4,14),(4,4),(5,1),(7,0),(10,0),(12,1),(15,4)]
      ,[(15,0),(15,14)]]))
  , ('v',(12,[[(2,14),(8,0),(14,14)]]))
  , ('w',(18,[[(3,14),(7,0),(11,14),(15,0),(19,14)]]))
  , ('x',(13,[[(3,0),(14,14)],[(3,14),(14,0)]]))
  , ('y',(12,[[(2,14),(8,0)],[(1,-7),(2,-7),(4,-6),(6,-4),(8,0),(14,14)]]))
  , ('z',(13,[[(3,14),(14,14),(3,0),(14,0)]]))
  , ('{',(10,[[(7,-6),(6,-4),(6,-2),(7,0),(8,1),(9,3),(9,5),(8,7),(4,9),(8,11)
      ,(9,13),(9,15),(8,17),(7,18),(6,20),(6,22),(7,24)],[(9,25),(7,24),(6,23)
      ,(5,21),(5,19),(6,17),(7,16),(8,14),(8,12),(6,10)],[(6,8),(8,6),(8,4)
      ,(7,2),(6,1),(5,-1),(5,-3),(6,-5),(7,-6),(9,-7)]]))
  , ('|',(6,[[(4,-7),(4,25)]]))
  , ('}',(10,[[(5,-7),(7,-6),(8,-5),(9,-3),(9,-1),(8,1),(7,2),(6,4),(6,6),(8,8)]
      ,[(8,10),(6,12),(6,14),(7,16),(8,17),(9,19),(9,21),(8,23),(7,24),(5,25)]
      ,[(7,24),(8,22),(8,20),(7,18),(6,17),(5,15),(5,13),(6,11),(10,9),(6,7)
      ,(5,5),(5,3),(6,1),(7,0),(8,-2),(8,-4),(7,-6)]]))
  , ('~',(20
      ,[[(3,6),(3,8),(4,11),(6,12),(8,12),(10,11)
      ,(14,8),(16,7),(18,7),(20,8),(21,10)]
      ,[(21,12),(21,10),(20,7),(18,6),(16,6),(14,7)
      ,(10,10),(8,11),(6,11),(4,10),(3,8)]]))
  ]
{- FOURMOLU_ENABLE -}
