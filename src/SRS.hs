module SRS where

import Board
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Tetromino
import Types

getOffsets :: Tetromino -> (Orientation, Orientation) -> [Point]
getOffsets tetromino (from, to) = zipWith (<->) fromOffsets toOffsets
  where
    fromOffsets = fromJust $ Map.lookup from $ offsets tetromino
    toOffsets = fromJust $ Map.lookup to $ offsets tetromino

tryOffset :: Board -> Tetromino -> Point -> Bool
tryOffset board tetromino offset =
  canPlace board $ moveTetromino offset tetromino

-- Make strict in the future potentially
tryOffsets :: Board -> Tetromino -> [Point] -> Maybe Tetromino
tryOffsets board tetromino = f Nothing
  where
    f :: Maybe Tetromino -> [Point] -> Maybe Tetromino
    f (Just t) _ = Just t
    f _ [] = Nothing
    f _ (x:xs) =
      if tryOffset board tetromino x
        then Just $ moveTetromino x tetromino
        else f Nothing xs

tryRotate :: Board -> Tetromino -> Direction -> Maybe Tetromino
tryRotate board tetromino dir = tryOffsets board tetromino' tOffsets
  where
    tetromino' = rotate dir tetromino
    tOffsets = getOffsets tetromino' orientations
    orientations = (orientation tetromino, orientation tetromino')
