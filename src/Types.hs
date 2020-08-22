module Types where

class HasEmpty a where
  empty :: a
  isEmpty :: a -> Bool

newtype Mino =
  Mino
    { isFilled :: Bool
    }
  deriving (Eq)

instance HasEmpty Mino where
  empty = Mino False
  isEmpty = not . isFilled

instance Show Mino where
  show (Mino True) = "*"
  show (Mino False) = "-"

type Point = (Int, Int)

type Delta = Point

type Offset = Point

data Rotation
  = CW
  | CCW
  deriving (Show, Eq, Ord)

data Direction
  = L
  | R
  deriving (Show, Eq, Ord)


data Input
  = Move Direction
  | Rotate Rotation
  | ForceDown
  | Nop
  -- | Gravity
  deriving (Show, Eq, Ord)

infixl 6 <+>, <->

(<+>) :: Point -> Point -> Point
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(<->) :: Point -> Point -> Point
(<->) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
