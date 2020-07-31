module Tetromino
  ( mkTetromino
  , Piece(..)
  , Tetromino(..)
  , Orientation(..)
  , minosPositions
  , moveTetromino
  , rotateTetromino
  ) where

import Types

data Piece
  = I
  | J
  | L
  | T
  | S
  | Z
  | O
  deriving (Show, Eq, Ord)

data Orientation
  = Zero
  | Right'
  | Two
  | Left'
  deriving (Show, Eq, Ord)

data Tetromino =
  Tetromino
    { piece :: Piece
    , orientation :: Orientation
    , pos :: Point
    , localPos :: [Point]
    , offsets :: [(Orientation, [Point])]
    }
  deriving (Show)

minosPositions :: Tetromino -> [Point]
minosPositions tetromino = (<+> p) <$> rotateMino o <$> localPos tetromino
  where
    p = pos tetromino
    o = orientation tetromino

mkTetromino :: Piece -> Tetromino
mkTetromino piece = Tetromino piece Zero (0, 0) positions offsets
  where
    positions = initialPositions piece
    offsets = pieceOffsets piece

moveTetromino :: Point -> Tetromino -> Tetromino
moveTetromino point tetromino = tetromino {pos = pos tetromino <+> point}

rotateTetromino :: Orientation -> Tetromino -> Tetromino
rotateTetromino o tetromino = tetromino {orientation = o}

rotateMino :: Orientation -> Point -> Point
rotateMino Zero (x, y) = (x, y)
rotateMino Right' (x, y) = (y, -x)
rotateMino Two (x, y) = (-x, -y)
rotateMino Left' (x, y) = (-y, x)

initialPositions :: Piece -> [Point]
initialPositions I = [(-1, 0), (0, 0), (1, 0), (2, 0)]
initialPositions J = [(-1, 1), (-1, 0), (0, 0), (1, 0)]
initialPositions L = [(-1, 0), (0, 0), (1, 0), (1, 1)]
initialPositions T = [(-1, 0), (0, 0), (1, 0), (0, 1)]
initialPositions S = [(-1, 0), (0, 0), (0, 1), (1, 1)]
initialPositions Z = [(-1, 1), (0, 1), (0, 0), (1, 0)]
initialPositions O = [(0, 0), (1, 0), (0, 1), (1, 1)]

pieceOffsets :: Piece -> [(Orientation, [Point])]
pieceOffsets I =
  [ (Zero, [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)])
  , (Right', [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)])
  , (Two, [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)])
  , (Left', [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)])
  ]
pieceOffsets O =
  [(Zero, [(0, 0)]), (Right', [(0, -1)]), (Two, [(-1, -1)]), (Left', [(-1, 0)])]
pieceOffsets _ =
  [ (Zero, [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)])
  , (Right', [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)])
  , (Two, [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)])
  , (Left', [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)])
  ]
