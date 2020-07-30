module Tetromino
  ( mkTetromino
  , Mino(..)
  ) where

import Board

newtype Mino =
  Mino
    { isFilled :: Bool
    }
  deriving (Show, Read)

instance HasEmpty Mino where
  empty = Mino False
  isEmpty = not . isFilled

-- instance Show Mino where
--   show (Mino True) = "*"
--   show (Mino False) = "-"
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
  | Right
  | Two
  | Left
  deriving (Show, Eq, Ord)

data Tetromino =
  Tetromino
    { piece :: Piece
    , pos :: Point
    , orientation :: Orientation
    }

mkTetromino :: Piece -> Point -> Orientation -> Tetromino
mkTetromino = Tetromino
