module Utils
  ( printBoard
  , writeBoard
  , readBoard
  , readMino
  ) where

import Board
import Control.Monad
import System.Console.ANSI
import Tetromino

printBoard :: Board Mino -> IO ()
printBoard board = do
  sequence_ $
    fmap printRow . (fmap . fmap) printMino . fmap words . lines . show $ board
  setSGR [SetColor Foreground Dull White]
  where
    printRow actions = sequence actions >> putStrLn ""
    printMino minoStr = do
      let mino@(Mino filled) = readMino minoStr
      let intensity =
            if filled
              then Vivid
              else Dull
      let color =
            if filled
              then Green
              else White
      let minoPrint =
            if filled
              then "*"
              else " "
      setSGR [SetColor Foreground intensity color] >>
        (putStr $ minoPrint ++ " ")

writeBoard :: FilePath -> Board Mino -> IO ()
writeBoard path board = writeFile path . show $ board

readBoard :: FilePath -> IO (Board Mino)
readBoard path = do
  result <- readFile path
  return $
    boardFromList . reverse . (fmap . fmap) readMino . fmap words . lines $
    result

readMino :: String -> Mino
readMino str
  | str == show (Mino True) = Mino True
  | str == show (Mino False) = Mino False
  | otherwise = error "Can only read boards with '*' and '-' as minos" -- TODO: Use Maybe
