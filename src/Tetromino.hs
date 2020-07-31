module Tetromino
  ( mkTetromino
  , Mino(..)
  , Piece(..)
  ) where

import Board

newtype Mino =
  Mino
    { isFilled :: Bool
    }

instance HasEmpty Mino where
  empty = Mino False
  isEmpty = not . isFilled

instance Show Mino where
  show (Mino True) = "*"
  show (Mino False) = "-"

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

mkTetromino :: Piece -> Tetromino
mkTetromino piece = Tetromino piece Zero (0, 0) positions offsets
  where
    positions = initialPositions piece
    offsets = pieceOffsets piece

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
