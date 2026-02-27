{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Brillo
import Brillo.Interface.Pure.Game
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Text qualified as T


{- Chess game between two human players on a single computere.
   Game rules is verified durint play, but draw and checkmate is not detected.
   Compile: ghc -O2 Chess.hs
   Optimize: strip Chess -o ChessStrip
   Run: ./Chess -}

main :: IO ()
main = do
  let
    boardWidth = 500
    maxTime = 900
  play
    (InWindow (T.pack "Haskell Chess") (600, 650) (0, 0))
    (makeColorI 180 120 70 255) -- Wood brown background
    5
    (initialState maxTime)
    (drawWorld boardWidth)
    (handleEvent boardWidth)
    stepWorld


type BoardWidth = Float
type Board = Map.Map Pos Piece
type Pos = (Int, Int) -- (col, row)
data PColor = White | Black deriving (Eq, Show)
data PType = King | Rook | Bishop | Queen | Knight | Pawn deriving (Eq, Show)
data PState = Init | TwoStep | Moved deriving (Eq, Show)
data Piece = Piece
  { ptype :: PType
  , pcolor :: PColor
  , pstate :: PState
  }
  deriving (Eq, Show)
data Marker = Marker
  { position :: Pos
  , selected :: Maybe Pos
  , invalid :: Maybe Pos
  }
data Player = Player
  { name :: T.Text -- Not yet used
  , timeLeft :: Float
  , col :: PColor -- Not yet used
  , captured :: [Piece] -- Not yet used
  }
data State = State
  { board :: Board
  , marker :: Marker
  , whitePlayer :: Player
  , blackPlayer :: Player
  , current :: PColor
  , message :: T.Text
  }


tBlue :: Color
tBlue = makeColorI 70 110 180 255


tGreen :: Color
tGreen = makeColorI 80 160 80 255


tRed :: Color
tRed = makeColorI 180 60 60 255


-- Helpers to handle pieces on the board
setPiece :: Pos -> Piece -> Board -> Board
setPiece = Map.insert


updatePiece :: (Piece -> Piece) -> Pos -> Board -> Board
updatePiece = Map.adjust


getPiece :: Pos -> Board -> Maybe Piece
getPiece = Map.lookup


dropPiece :: Pos -> Board -> Board
dropPiece = Map.delete


movePiece :: Pos -> Pos -> Board -> Board
movePiece from to b = setPiece to p $ dropPiece from b
  where
    p = fromJust $ getPiece from b


setMarker :: Pos -> State -> State
setMarker p s = s{marker = (marker s){position = p, invalid = Nothing}}


setMessage :: T.Text -> State -> State
setMessage m s = s{message = m}


-- Set selected if marker is on a valid piece
setSelected :: Maybe Pos -> State -> State
setSelected Nothing s = s{marker = (marker s){selected = Nothing, invalid = Nothing}}
setSelected (Just p) s = if valid then sValid else sInvalid
  where
    piece = getPiece p (board s)
    valid = isJust piece && (pcolor (fromJust piece) == current s)
    hasPiece = isJust piece
    sValid = s{marker = (marker s){selected = Just p, invalid = Nothing}}
    sInvalid = s{marker = (marker s){invalid = if hasPiece then Just p else Nothing}}


-- Move pos marker, but make sure move is not outside board
movePos :: Pos -> SpecialKey -> Pos
movePos (a, b) KeyLeft = if (a > 0) then (a - 1, b) else (a, b)
movePos (a, b) KeyRight = if (a < 7) then (a + 1, b) else (a, b)
movePos (a, b) KeyUp = if (b > 0) then (a, b - 1) else (a, b)
movePos (a, b) KeyDown = if (b < 7) then (a, b + 1) else (a, b)
movePos p _ = p


-- Move piece according to marker position if valid move. Assumes that a piece has been selected
-- This happen when the user has selected a piece and drop it on a new location
drawPiece :: State -> State
drawPiece s
  | (isNothing b') =
      setMessage
        (T.pack "Invalid")
        s{marker = (marker s){invalid = Just (position $ marker s)}}
  | (isCheck s') = setMessage (T.pack "Check") s
  | otherwise = setMessage (T.pack "") $ stepCurrent s'
  where
    b = board s
    b' = stepBoard b
    s' = s{board = fromJust b'}

    -- Update board state if possible
    stepBoard :: Board -> Maybe Board
    stepBoard _boardState
      | isEnPassant = Just $ movePiece p0 p1 $ dropPiece (fst p1, snd p0) b
      | isPromotion = Just $ movePiece p0 p1 $ updatePiece (promotePiece Queen) p0 b
      | isCapture = Just $ movePiece p0 p1 $ updatePiece (stepPState p0 p1) p0 b
      | isCastling =
          let (r0, r1) = castlingRookMove (p0, p1)
          in  Just $ movePiece r0 r1 $ movePiece p0 p1 b
      | isMove = Just $ movePiece p0 p1 $ updatePiece (stepPState p0 p1) p0 b
      | otherwise = Nothing

    (p0, p1) = (fromJust $ selected $ marker s, position $ marker s)
    piece = fromJust $ getPiece p0 b
    capture = getPiece p1 b
    isValid =
      (pcolor piece == current s)
        && (distance p0 p1 > 0)
        && (isJust $ getPiece p0 b)
    isPathClear = pathClear b p0 p1 piece
    isMove =
      isValid
        && isPathClear
        && (isNothing $ getPiece p1 b)
        && (validMove piece p0 p1)
    isCapture =
      isValid
        && isPathClear
        && (isJust $ capture)
        && (pcolor $ fromJust $ capture) /= (current s)
        && (validCapture piece p0 p1)
    isEnPassant =
      let cpawn = getPiece (fst p1, snd p0) b
      in  (isJust cpawn)
            && ((pstate $ fromJust cpawn) == TwoStep) -- there's a piece to capture
            && (validCapture piece p0 p1) -- there's a pawm that has moved TwoStep
    isPromotion =
      (isMove || isCapture)
        && (ptype piece == Pawn)
        && (snd p1 == 0 || snd p1 == 7) -- it's a pawn
        -- move is to final line
    isCastling =
      let (r0, _r1) = castlingRookMove (p0, p1)
          rook = getPiece r0 b
      in  isValid
            && (pathClear b p0 r0 piece)
            && (ptype piece == King) -- check that path is clear
            && (pstate piece == Init) -- check that piece is the king
            && (isJust $ rook) -- check that king has not moved
            && ((pstate $ fromJust rook) == Init) -- check that rook has not moved
            && (fst p1 == 2 || fst p1 == 6) -- check that target square is correct


-- Check if a move from p0 to p1 is valid on the given board for the given color
canMove :: Board -> PColor -> Pos -> Pos -> Bool
canMove b cur p0 p1 =
  case getPiece p0 b of
    Nothing -> False
    Just piece ->
      let capture = getPiece p1 b
          isValid =
            (pcolor piece == cur)
              && (distance p0 p1 > 0)
          isPathClear = pathClear b p0 p1 piece
          isAMove =
            isValid
              && isPathClear
              && isNothing capture
              && validMove piece p0 p1
          isCapture =
            isValid
              && isPathClear
              && isJust capture
              && pcolor (fromJust capture) /= cur
              && validCapture piece p0 p1
          isEnPassant =
            let cpawn = getPiece (fst p1, snd p0) b
            in  isJust cpawn
                  && (pstate (fromJust cpawn) == TwoStep)
                  && validCapture piece p0 p1
          isCastling =
            let (r0, _r1) = castlingRookMove (p0, p1)
                rook = getPiece r0 b
            in  isValid
                  && pathClear b p0 r0 piece
                  && ptype piece == King
                  && pstate piece == Init
                  && isJust rook
                  && (pstate (fromJust rook) == Init)
                  && (fst p1 == 2 || fst p1 == 6)
      in  isAMove || isCapture || isEnPassant || isCastling


-- Get rook move from king move in castling
castlingRookMove :: (Pos, Pos) -> (Pos, Pos)
castlingRookMove (p0, p1) =
  let r = snd p0
  in  if (fst p0 < fst p1)
        then ((7, r), (5, r))
        else ((0, r), (3, r))


-- Step PState according to move from a to b
stepPState :: Pos -> Pos -> Piece -> Piece
stepPState a b p =
  if (ptype p == Pawn && distance a b == 2)
    then p{pstate = TwoStep}
    else p{pstate = Moved}


-- Board distance in number of (diagonal or horizontal steps
distance :: Pos -> Pos -> Int
distance (c0, r0) (c1, r1) = max (abs $ c1 - c0) (abs $ r1 - r0)


-- Check that board is clear between two board positions (either horizontal or diagonal)
pathClear :: Board -> Pos -> Pos -> Piece -> Bool
pathClear b (c0, r0) (c1, r1) p
  | (ptype p == Knight) = True
  | (d > 1) = (length blockers) == 0
  | otherwise = True
  where
    d = distance (c0, r0) (c1, r1)
    (sc, sr) = ((c1 - c0) `div` d, (r1 - r0) `div` d)
    ps = map (\s -> (c0 + (sc * s), r0 + (sr * s))) [1 .. (d - 1)]
    blockers = catMaybes $ map (`getPiece` b) ps


-- Check if move piece from a to b is a valid move (assuming an empty board)
validMove :: Piece -> Pos -> Pos -> Bool
validMove (Piece King _ _) (c0, r0) (c1, r1) = abs (c1 - c0) <= 1 && abs (r1 - r0) <= 1
validMove (Piece Rook _ _) (c0, r0) (c1, r1) = (c1 - c0) == 0 || (r1 - r0) == 0
validMove (Piece Bishop _ _) (c0, r0) (c1, r1) = abs (c1 - c0) == abs (r1 - r0)
validMove (Piece Queen _ _) p0 p1 =
  validMove (Piece Rook Black Init) p0 p1
    || validMove (Piece Bishop Black Init) p0 p1
validMove (Piece Knight _ _) (c0, r0) (c1, r1) =
  let (dc, dr) = (abs (c1 - c0), abs (r1 - r0))
  in  (dc == 1 && dr == 2) || (dc == 2 && dr == 1)
validMove (Piece Pawn Black _) (c0, r0) (c1, r1) = (c1 - c0) == 0 && ((r1 - r0) == 1 || ((r1 - r0) == 2 && r0 == 1))
validMove (Piece Pawn White _) (c0, r0) (c1, r1) = (c1 - c0) == 0 && ((r0 - r1) == 1 || ((r0 - r1) == 2 && r0 == 6))


validCapture :: Piece -> Pos -> Pos -> Bool
validCapture (Piece Pawn Black _) (c0, r0) (c1, r1) = (r1 - r0) == 1 && abs (c1 - c0) == 1
validCapture (Piece Pawn White _) (c0, r0) (c1, r1) = (r0 - r1) == 1 && abs (c1 - c0) == 1
validCapture p p0 p1 = validMove p p0 p1


-- Determine if current player is checked. Assumes that the king exist for cur player
isCheck :: State -> Bool
isCheck s = not $ Map.null checkers
  where
    b = board s
    c = current s
    kingPositions =
      Map.keys
        $ Map.filter
          ( \p ->
              ((ptype p) == King)
                && ((pcolor p) == c)
          )
        $ b
    kingPos = case kingPositions of
      (k : _) -> k
      [] -> (0, 0) -- Should never happen in a valid game
    checkers =
      Map.filterWithKey
        ( \checkerPos p ->
            (pcolor p /= c)
              && (pathClear b checkerPos kingPos p)
              && (validCapture p checkerPos kingPos)
        )
        $ b


-- Change current player and clear marker and
stepCurrent :: State -> State
stepCurrent s =
  s
    { marker = (marker s){selected = Nothing, invalid = Nothing}
    , current = opposite (current s)
    }
  where
    opposite col = if col == Black then White else Black


promotePiece :: PType -> Piece -> Piece
promotePiece new p = p{ptype = new}


-- Draw the scene
drawWorld :: BoardWidth -> State -> Picture
drawWorld bw s =
  Translate (-bw * 0.5) (-bw * 0.55) $
    Pictures $
      [ drawBoard bw
      , drawPieces bw (board s)
      , drawMarker bw s
      , drawSelected bw (marker s)
      , drawInvalid bw (marker s)
      , drawState bw s
      ]


-- Draw board with bottom left corner in origo
drawBoard :: BoardWidth -> Picture
drawBoard w =
  let
    tup a b = (a, b)
    sqAt (c, r) = Translate c r $ rectangleSolid 1 1
    blackSquares =
      (tup <$> [0, 2, 4, 6] <*> [0, 2, 4, 6])
        ++ (tup <$> [1, 3, 5, 7] <*> [1, 3, 5, 7])
    whiteSquares =
      (tup <$> [1, 3, 5, 7] <*> [0, 2, 4, 6])
        ++ (tup <$> [0, 2, 4, 6] <*> [1, 3, 5, 7])
    border = Translate 3.5 3.5 $ rectangleWire 8 8
  in
    Scale (w / 8) (w / 8) $
      Translate 0.5 0.5 $
        Pictures $
          [ Color white $ Pictures (map sqAt whiteSquares)
          , Color (greyN 0.5) $ Pictures (map sqAt blackSquares)
          , border
          ]


drawPieces :: BoardWidth -> Board -> Picture
drawPieces bw b =
  Translate (bw / 16) (bw / 16) $
    Pictures $
      Map.elems $
        Map.mapWithKey (\pos p -> toPic pos p) b
  where
    toPic :: Pos -> Piece -> Picture
    toPic pos p =
      let (tx, ty) = translatePos bw $ pos
      in  Translate tx ty $ pieceGfx p


-- Draw marker
drawMarker :: BoardWidth -> State -> Picture
drawMarker bw s =
  let pos = position $ marker s
      (tx, ty) = translatePos bw pos
      piece = getPiece pos (board s)
      isEnemy = case piece of
        Just p -> pcolor p /= current s
        Nothing -> False
      sel = selected $ marker s
      noSelection = isNothing sel
      invalidMove = case sel of
        Just from -> pos /= from && not (canMove (board s) (current s) from pos)
        Nothing -> False
      col
        | isEnemy && noSelection = tRed
        | noSelection && isNothing piece = makeColorI 140 90 50 255
        | invalidMove = tRed
        | otherwise = tGreen
      ring = Color col $ Translate tx ty $ ThickCircle 27 6
  in  Translate (bw / 16) (bw / 16) $ ring


-- Draw selected
drawSelected :: BoardWidth -> Marker -> Picture
drawSelected bw m =
  let sel = selected m
      (tx, ty) = translatePos bw $ maybe (0, 0) id sel
      selector = Color tBlue $ Translate tx ty $ ThickCircle 27 6
  in  if (isJust sel)
        then Translate (bw / 16) (bw / 16) $ selector
        else Blank


-- Draw invalid selection
drawInvalid :: BoardWidth -> Marker -> Picture
drawInvalid bw m =
  let inv = invalid m
      (tx, ty) = translatePos bw $ maybe (0, 0) id inv
      ring = Color tRed $ Translate tx ty $ ThickCircle 27 6
  in  if (isJust inv)
        then Translate (bw / 16) (bw / 16) $ ring
        else Blank


-- Translete to a board position, assuming that center of bottom, left square is in origo
translatePos :: BoardWidth -> Pos -> (Float, Float)
translatePos bw (c, r) = ((bw / 8) * fromIntegral c, (bw / 8) * fromIntegral (7 - r))


-- Draw current player state
drawState :: BoardWidth -> State -> Picture
drawState bw s =
  let
    col = case (current s) of
      Black -> black
      White -> white
    pCurrent =
      Translate (0.5 * bw) (1.1 * bw) $
        Pictures
          [ Color col $ rectangleSolid (bw / 16) (bw / 16)
          , Color black $ rectangleWire (bw / 16) (bw / 16)
          ]
    tWhite = round (timeLeft . whitePlayer $ s) :: Integer
    tBlack = round (timeLeft . blackPlayer $ s) :: Integer
    pWhite =
      Color (timeColor tWhite) $
        Translate (0.3 * bw) (1.075 * bw) $
          Scale 0.2 0.2 $
            Text $
              T.pack $
                secFormatted $
                  tWhite
    pBlack =
      Color (timeColor tBlack) $
        Translate (0.56 * bw) (1.075 * bw) $
          Scale 0.2 0.2 $
            Text $
              T.pack $
                secFormatted $
                  tBlack
    secFormatted sec = show (sec `div` 60) ++ ":" ++ show (sec `mod` 60)
    timeColor sec = if (sec < 0) then red else black
    pMessage =
      Color black $
        Translate (0.05 * bw) ((-0.06) * bw) $
          Scale 0.15 0.15 $
            Text $
              message s
  in
    Pictures $ [pCurrent, pWhite, pBlack, pMessage]


initialState :: Float -> State
initialState timeLeft =
  State
    (Map.fromList board)
    marker
    whitePlayer
    blackPlayer
    White
    (T.pack "Arrows/Click: Move | Space/Click: Select")
  where
    asBlack t = Piece t Black Init
    asWhite t = Piece t White Init
    atRow r c = (c, r)
    r0 = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    r1 = [Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn]
    board =
      zip (map (atRow 0) [0 .. 7]) (map asBlack r0)
        ++ zip (map (atRow 1) [0 .. 7]) (map asBlack r1)
        ++ zip (map (atRow 6) [0 .. 7]) (map asWhite r1)
        ++ zip (map (atRow 7) [0 .. 7]) (map asWhite r0)
    marker = Marker (0, 0) Nothing Nothing
    blackPlayer = Player (T.pack "Black Player") timeLeft Black []
    whitePlayer = Player (T.pack "White Player") timeLeft White []


-- Convert world-space click coordinates to board position
pixelToBoard :: BoardWidth -> (Float, Float) -> Maybe Pos
pixelToBoard bw (mx, my) =
  let col = floor ((mx + bw * 0.5) / (bw / 8))
      row = 7 - floor ((my + bw * 0.55) / (bw / 8))
  in  if col >= 0 && col <= 7 && row >= 0 && row <= 7
        then Just (col, row)
        else Nothing


handleEvent :: BoardWidth -> Event -> State -> State
handleEvent _ (EventKey (SpecialKey KeyUp) Down _ _) s = setMarker (movePos (position . marker $ s) KeyUp) s
handleEvent _ (EventKey (SpecialKey KeyDown) Down _ _) s = setMarker (movePos (position . marker $ s) KeyDown) s
handleEvent _ (EventKey (SpecialKey KeyLeft) Down _ _) s = setMarker (movePos (position . marker $ s) KeyLeft) s
handleEvent _ (EventKey (SpecialKey KeyRight) Down _ _) s = setMarker (movePos (position . marker $ s) KeyRight) s
handleEvent _ (EventKey (SpecialKey KeySpace) Down _ _) s -- Space toggle selection, or draw piece
  | isNothing sel = setSelected (Just pos) s
  | pos == fromJust sel = setSelected Nothing s
  | otherwise = drawPiece s
  where
    sel = selected . marker $ s
    pos = position . marker $ s
handleEvent bw (EventKey (MouseButton LeftButton) Down _ clickPos) s
  | Just p <- pixelToBoard bw clickPos =
      let s' = setMarker p s
          sel = selected . marker $ s'
      in  if isNothing sel
            then setSelected (Just p) s'
            else
              if p == fromJust sel
                then setSelected Nothing s'
                else drawPiece s'
handleEvent _ _ s = s


stepWorld :: Float -> State -> State
stepWorld dt s@(State _ _ w _ White _) = s{whitePlayer = w{timeLeft = (timeLeft w - dt)}}
stepWorld dt s@(State _ _ _ b Black _) = s{blackPlayer = b{timeLeft = (timeLeft b - dt)}}


-- Drawn chess piece graphics
pieceGfx :: Piece -> Picture
pieceGfx (Piece t c _) =
  let
    fill = case c of
      White -> greyN 0.9
      Black -> greyN 0.15
    outline = case c of
      White -> greyN 0.2
      Black -> greyN 0.7
  in
    case t of
      Pawn -> pawnPic fill outline
      Rook -> rookPic fill outline
      Knight -> knightPic fill outline
      Bishop -> bishopPic fill outline
      Queen -> queenPic fill outline
      King -> kingPic fill outline


-- Draw a filled polygon with a border via offset copies
borderedPoly :: Color -> Color -> [Point] -> Picture
borderedPoly fill bord pts =
  Pictures
    [ Color bord $
        Pictures
          [Translate dx dy $ Polygon pts | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    , Color fill $ Polygon pts
    ]


-- Draw a filled circle with a border
borderedCircle :: Color -> Color -> Float -> Picture
borderedCircle fill bord r =
  Pictures
    [ Color bord $ circleSolid (r + 1.5)
    , Color fill $ circleSolid r
    ]


pawnPic :: Color -> Color -> Picture
pawnPic f o =
  Pictures
    [ borderedPoly
        f
        o
        [ (-4, 3)
        , (-8, -8)
        , (-10, -13)
        , (-10, -15)
        , (10, -15)
        , (10, -13)
        , (8, -8)
        , (4, 3)
        ]
    , Translate 0 8 $ borderedCircle f o 6
    ]


rookPic :: Color -> Color -> Picture
rookPic f o =
  borderedPoly
    f
    o
    [ (-11, -15)
    , (-11, -10)
    , (-8, -10)
    , (-8, 8)
    , (-11, 8)
    , (-11, 15)
    , (-7, 15)
    , (-7, 11)
    , (-2, 11)
    , (-2, 15)
    , (2, 15)
    , (2, 11)
    , (7, 11)
    , (7, 15)
    , (11, 15)
    , (11, 8)
    , (8, 8)
    , (8, -10)
    , (11, -10)
    , (11, -15)
    ]


knightPic :: Color -> Color -> Picture
knightPic f o =
  borderedPoly
    f
    o
    [ (-6, -15)
    , (-6, 0)
    , (-8, 6)
    , (-6, 10)
    , (-2, 14)
    , (2, 15)
    , (6, 12)
    , (8, 6)
    , (10, 2)
    , (8, -2)
    , (4, 0)
    , (6, -8)
    , (8, -15)
    ]


bishopPic :: Color -> Color -> Picture
bishopPic f o =
  Pictures
    [ borderedPoly
        f
        o
        [ (0, 14)
        , (-6, 4)
        , (-9, -6)
        , (-10, -13)
        , (-10, -15)
        , (10, -15)
        , (10, -13)
        , (9, -6)
        , (6, 4)
        ]
    , Translate 0 17 $ borderedCircle f o 3
    ]


queenPic :: Color -> Color -> Picture
queenPic f o =
  borderedPoly
    f
    o
    [ (-11, -15)
    , (-9, -6)
    , (-7, 0)
    , (-11, 10)
    , (-6, 6)
    , (-3, 13)
    , (0, 8)
    , (3, 13)
    , (6, 6)
    , (11, 10)
    , (7, 0)
    , (9, -6)
    , (11, -15)
    ]


kingPic :: Color -> Color -> Picture
kingPic f o =
  Pictures
    [ borderedPoly
        f
        o
        [ (-10, -15)
        , (-9, -8)
        , (-7, 0)
        , (-6, 8)
        , (6, 8)
        , (7, 0)
        , (9, -8)
        , (10, -15)
        ]
    , borderedPoly
        f
        o
        [(-1.5, 8), (-1.5, 20), (1.5, 20), (1.5, 8)]
    , borderedPoly
        f
        o
        [(-5, 14), (-5, 17), (5, 17), (5, 14)]
    ]
