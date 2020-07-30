module Utils
  ( printBoard
  , writeBoard
  , readBoard
  ) where

import Board
import Tetromino

printBoard :: Board Mino -> IO ()
printBoard = putStrLn . showBoard

writeBoard :: FilePath -> Board Mino -> IO ()
writeBoard path board = writeFile path . showBoard $ board
  where
    writeMino (Mino True) = "*"
    writeMino (Mino False) = "-"

readBoard :: FilePath -> IO (Board Mino)
readBoard path = do
  result <- readFile path
  return $
    boardFromList . reverse . (fmap . fmap) readMino . fmap words . lines $
    result
  where
    readMino "*" = Mino True
    readMino "-" = Mino False
    readMino _ = error "Can only read boards with '*' and '-' as minos" -- TODO: Use Maybe

showBoard :: Board Mino -> String
showBoard =
  unlines . fmap unwords . (fmap . fmap) writeMino . reverse . boardToList
  where
    writeMino (Mino True) = "*"
    writeMino (Mino False) = "-"
