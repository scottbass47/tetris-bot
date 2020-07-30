module Lib
  ( runBot
  ) where

import Board
import Tetromino
import Utils

runBot :: IO ()
runBot = do
  let width = 10
  board <- readBoard "res/board.txt"
  -- let board =
  --       mkBoard' 20 width (\(r, c) -> Mino ((r * width + c + r) `mod` 2 == 0))
  -- writeBoard "res/board.txt" board
  printBoard $ clearRows [0, 1, 2] board
