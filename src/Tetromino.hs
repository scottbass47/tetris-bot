{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tetromino where

import Data.Aeson.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Tuple (swap)
import GHC.Generics
import Types hiding (Direction (L))

data Piece
  = I
  | J
  | L
  | T
  | S
  | Z
  | O
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Orientation
  = Zero
  | Right'
  | Two
  | Left'
  deriving (Show, Eq, Ord)

instance FromJSON Orientation where
  parseJSON = withText "Orientation" $ \s ->
    case s of
      "right" -> return Right'
      "left" -> return Left'
      "two" -> return Two
      "zero" -> return Zero
      _ -> fail $ T.unpack $ "Orientation " <> s <> " doesn't exist"

instance ToJSON Orientation where
  toJSON Left' = "left"
  toJSON Right' = "right"
  toJSON Two = "two"
  toJSON Zero = "zero"

data Tetromino = Tetromino
  { piece :: Piece,
    orientation :: Orientation,
    pos :: Point,
    localPos :: [Point],
    offsets :: Map.Map Orientation [Point]
  }
  deriving (Show, Ord, Eq)

minosPositions :: Tetromino -> [Point]
minosPositions tetromino =
  (swap . (<+> p)) . rotateMino o <$> localPos tetromino
  where
    p = pos tetromino
    o = orientation tetromino

mkTetromino :: Piece -> Tetromino
mkTetromino piece = Tetromino piece Zero (0, 0) positions offsets
  where
    positions = initialPositions piece
    offsets = pieceOffsets piece

moveTetromino :: Delta -> Tetromino -> Tetromino
moveTetromino point tetromino = tetromino {pos = pos tetromino <+> point}

rotateTetromino :: Orientation -> Tetromino -> Tetromino
rotateTetromino o tetromino = tetromino {orientation = o}

rotateMino :: Orientation -> Point -> Point
rotateMino Zero (x, y) = (x, y)
rotateMino Right' (x, y) = (y, - x)
rotateMino Two (x, y) = (- x, - y)
rotateMino Left' (x, y) = (- y, x)

rotate :: Rotation -> Tetromino -> Tetromino
rotate dir =
  case dir of
    CW -> rotateCW
    CCW -> rotateCCW

rotateCW :: Tetromino -> Tetromino
rotateCW tetromino = rotateTetromino newOrientation tetromino
  where
    newOrientation =
      case orientation tetromino of
        Zero -> Right'
        Right' -> Two
        Two -> Left'
        Left' -> Zero

rotateCCW :: Tetromino -> Tetromino
rotateCCW tetromino = rotateTetromino newOrientation tetromino
  where
    newOrientation =
      case orientation tetromino of
        Zero -> Left'
        Left' -> Two
        Two -> Right'
        Right' -> Zero

initialPositions :: Piece -> [Point]
initialPositions I = [(-1, 0), (0, 0), (1, 0), (2, 0)]
initialPositions J = [(-1, 1), (-1, 0), (0, 0), (1, 0)]
initialPositions L = [(-1, 0), (0, 0), (1, 0), (1, 1)]
initialPositions T = [(-1, 0), (0, 0), (1, 0), (0, 1)]
initialPositions S = [(-1, 0), (0, 0), (0, 1), (1, 1)]
initialPositions Z = [(-1, 1), (0, 1), (0, 0), (1, 0)]
initialPositions O = [(0, 0), (1, 0), (0, 1), (1, 1)]

pieceOffsets :: Piece -> Map.Map Orientation [Point]
pieceOffsets I =
  Map.fromList
    [ (Zero, [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)]),
      (Right', [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)]),
      (Two, [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)]),
      (Left', [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)])
    ]
pieceOffsets O =
  Map.fromList
    [ (Zero, [(0, 0)]),
      (Right', [(0, -1)]),
      (Two, [(-1, -1)]),
      (Left', [(-1, 0)])
    ]
pieceOffsets _ =
  Map.fromList
    [ (Zero, [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]),
      (Right', [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)]),
      (Two, [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]),
      (Left', [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)])
    ]
