{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import qualified Data.Text as T 

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

instance FromJSON Input where
  parseJSON = withText "Input" $ \s ->
    case s of
      "moveLeft" -> return $ Move Types.L
      "moveRight" -> return $ Move Types.R
      "rotateCW" -> return $ Rotate CW
      "rotateCCW" -> return $ Rotate CCW
      _ -> fail $ T.unpack $ "Input " <> s <> " not handled"

instance ToJSON Input where
  toJSON (Move Types.L) = "moveLeft"
  toJSON (Move Types.R) = "moveRight"
  toJSON (Rotate CW) = "rotateCW"
  toJSON (Rotate CCW) = "rotateCCW"
  toJSON input = String . T.pack . show $ input

infixl 6 <+>, <->

(<+>) :: Point -> Point -> Point
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(<->) :: Point -> Point -> Point
(<->) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
