{-# LANGUAGE InstanceSigs #-}

module Core
  (Board (getBoard) 
    , Mino (Mino, isFilled) 
    , mkBoard
    , mkBoardWithConstructor
    , dims
    -- , clearRow
  ) where

import Data.List
import Data.Array

class Empty a where
  empty :: a
  isEmpty :: a -> Bool

type Point = (Int, Int)
newtype Board a = Board { getBoard :: Array Point a }
newtype Mino = Mino { isFilled :: Bool }  

instance Empty Mino where
  empty = Mino False
  isEmpty = not . isFilled

instance Functor Board where
  fmap :: (a -> b) -> Board a -> Board b
  fmap = (<$>)

instance (Show a) => Show (Board a) where
  show :: Board a -> String
  show = unlines . fmap unwords . (fmap . fmap) show . reverse . boardToList  

instance Show Mino where
  show (Mino True) = "*"
  show (Mino False) = "-"

mkBoard :: Int -> Int -> a -> Board a
mkBoard r c val = mkBoardWithConstructor r c (\_ -> val) 

mkBoardWithConstructor :: Int -> Int -> (Point -> a) -> Board a
mkBoardWithConstructor r c f = Board $ array size [(i, f i) | i <- range size] 
  where size = ((0, 0), (r-1, c-1))

-- clearRow :: (Empty a) => Board a -> Int -> Board a
-- clearRow board row = Board r c $ removeRow row board ++ [emptyRow c]
--   where removeRow row = map snd . filter ((/=row) . fst) . zip [0..]

emptyRow :: (Empty a) => Int -> [a]
emptyRow width = replicate width empty

boardToList :: Board a -> [[a]]
boardToList (Board board) = (map . map) snd . groupBy equalRows . assocs $ board
  where equalRows x y = (fst . fst) x == (fst . fst) y

dims :: Board a -> Point
dims (Board board) = (maxRow - minRow + 1, maxCol - minCol + 1)
  where ((minRow, minCol), (maxRow, maxCol)) = bounds board
