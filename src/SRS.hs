module SRS where

import Board
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Tetromino
import Types

tryRotate :: Board -> Tetromino -> Rotation -> Maybe Tetromino
tryRotate board tetromino dir = tryOffsets board tetromino' tOffsets
  where
    tetromino' = rotate dir tetromino
    tOffsets = getOffsets tetromino' orientations
    orientations = (orientation tetromino, orientation tetromino')
    --
    getOffsets :: Tetromino -> (Orientation, Orientation) -> [Offset]
    getOffsets tetromino (from, to) = zipWith (<->) fromOffsets toOffsets
      where
        fromOffsets = fromJust $ Map.lookup from $ offsets tetromino
        toOffsets = fromJust $ Map.lookup to $ offsets tetromino
    --
    tryOffset :: Board -> Tetromino -> Offset -> Bool
    tryOffset board tetromino offset =
      canPlace board $ moveTetromino offset tetromino
    --
    -- Make strict in the future potentially
    tryOffsets :: Board -> Tetromino -> [Offset] -> Maybe Tetromino
    tryOffsets board tetromino = f Nothing
      where
        f :: Maybe Tetromino -> [Offset] -> Maybe Tetromino
        f (Just t) _ = Just t
        f _ [] = Nothing
        f _ (x:xs) =
          if tryOffset board tetromino x
            then Just $ moveTetromino x tetromino
            else f Nothing xs

tryMove :: Board -> Tetromino -> Delta -> Maybe Tetromino
tryMove board tetromino delta =
  if canPlace board t'
    then Just t'
    else Nothing
  where
    t' = moveTetromino delta tetromino
