{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board
  ( Board(getBoard)
  , mkBoard
  , mkBoard'
  , dims
  , clearRows
  , boardToList
  , boardFromList
  , placeTetromino
  ) where

import Data.Array
import Data.List
import Data.Tuple
import Tetromino
import Types

newtype Board =
  Board
    { getBoard :: Array Point Mino
    }

instance Show Board where
  show = unlines . fmap unwords . (fmap . fmap) show . reverse . boardToList

mkBoard :: Int -> Int -> Mino -> Board
mkBoard r c val = mkBoard' r c (\_ -> val)

mkBoard' :: Int -> Int -> (Point -> Mino) -> Board
mkBoard' r c f = Board $ array size [(i, f i) | i <- range size]
  where
    size = ((0, 0), (r - 1, c - 1))

clearRows :: [Int] -> Board -> Board
clearRows rows board = Board . (// emptyRows) . (// newRows) . getBoard $ board
  where
    (rowsToClear, rowsToKeep) =
      partition ((&&) <$> (`elem` rows) <*> inRange rowRange) $ range rowRange
    rowsToReplace = range (0, (length rowsToKeep) - 1)
    rowsToFill = range (length rowsToKeep, snd rowRange)
    rowRange = rowsRange board
    newRows =
      zipWith zipper (boardIndices rowsToKeep) (boardIndices rowsToReplace)
    emptyRows = zip (boardIndices rowsToFill) (repeat $ Mino False)
    boardIndices :: [Int] -> [Point]
    boardIndices rows =
      filter ((`elem` rows) . fst) . indices . getBoard $ board
    zipper :: Point -> Point -> (Point, Mino)
    zipper p1 p2 = (p2, (! p1) . getBoard $ board)

-- Assumes tetromino is in a valid position
placeTetromino :: Tetromino -> Board -> Board
placeTetromino tetromino (Board board) =
  Board $ board // (flip (,) (Mino True) . swap <$> minosPositions tetromino)

boardToList :: Board -> [[Mino]]
boardToList = (map . map) snd . groupBy equalRows . assocs . getBoard
  where
    equalRows ((x, _), _) ((y, _), _) = x == y

boardFromList :: [[Mino]] -> Board
boardFromList lst =
  Board $ listArray ((0, 0), (rows - 1, cols - 1)) $ concat lst
  where
    rows = length lst
    cols = head . fmap length $ lst -- Unsafe for empty list

dims :: Board -> Point
dims (Board board) = (maxRow - minRow + 1, maxCol - minCol + 1)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds board

rowsRange :: Board -> Point
rowsRange board = (minRow, maxRow)
  where
    ((minRow, _), (maxRow, _)) = bounds . getBoard $ board

colsRange :: Board -> Point
colsRange board = (minCol, maxCol)
  where
    ((_, minCol), (_, maxCol)) = bounds . getBoard $ board
