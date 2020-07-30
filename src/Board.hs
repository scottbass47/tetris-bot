{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board
  ( Board(getBoard)
  , mkBoard
  , mkBoard'
  , dims
  , clearRows
  , HasEmpty(..)
  , Point
  , boardToList
  , boardFromList
  ) where

import Data.Array
import Data.List
import Types

class HasEmpty a where
  empty :: a
  isEmpty :: a -> Bool

type Point = (Int, Int)

newtype Board a =
  Board
    { getBoard :: Array Point a
    }
  deriving (Show, Read)

instance Functor Board where
  fmap :: (a -> b) -> Board a -> Board b
  fmap f = Board . fmap f . getBoard

-- instance (Show a) => Show (Board a) where
--   show :: Board a -> String
--   show = unlines . fmap unwords . (fmap . fmap) show . reverse . boardToList
mkBoard :: Int -> Int -> a -> Board a
mkBoard r c val = mkBoard' r c (\_ -> val)

mkBoard' :: Int -> Int -> (Point -> a) -> Board a
mkBoard' r c f = Board $ array size [(i, f i) | i <- range size]
  where
    size = ((0, 0), (r - 1, c - 1))

clearRows ::
     forall a. (HasEmpty a)
  => [Int]
  -> Board a
  -> Board a
clearRows rows board = Board . (// emptyRows) . (// newRows) . getBoard $ board
  where
    (rowsToClear, rowsToKeep) =
      partition ((&&) <$> (`elem` rows) <*> inRange rowRange) $ range rowRange
    rowsToReplace = range (0, (length rowsToKeep) - 1)
    rowsToFill = range (length rowsToKeep, snd rowRange)
    rowRange = rowsRange board
    newRows =
      zipWith zipper (boardIndices rowsToKeep) (boardIndices rowsToReplace)
    emptyRows = zip (boardIndices rowsToFill) (repeat empty :: [a])
    boardIndices :: [Int] -> [Point]
    boardIndices rows =
      filter ((`elem` rows) . fst) . indices . getBoard $ board
    zipper :: Point -> Point -> (Point, a)
    zipper p1 p2 = (p2, (! p1) . getBoard $ board)

boardToList :: Board a -> [[a]]
boardToList = (map . map) snd . groupBy equalRows . assocs . getBoard
  where
    equalRows ((x, _), _) ((y, _), _) = x == y

boardFromList :: [[a]] -> Board a
boardFromList lst =
  Board $ listArray ((0, 0), (rows - 1, cols - 1)) $ concat lst
  where
    rows = length lst
    cols = head . fmap length $ lst

dims :: Board a -> Point
dims (Board board) = (maxRow - minRow + 1, maxCol - minCol + 1)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds board

rowsRange :: Board a -> Point
rowsRange board = (minRow, maxRow)
  where
    ((minRow, _), (maxRow, _)) = bounds . getBoard $ board

colsRange :: Board a -> Point
colsRange board = (minCol, maxCol)
  where
    ((_, minCol), (_, maxCol)) = bounds . getBoard $ board
