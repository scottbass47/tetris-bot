module Types where

class HasEmpty a where
  empty :: a
  isEmpty :: a -> Bool

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

type Point = (Int, Int)

infixl 6 <+>

(<+>) :: Point -> Point -> Point
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
