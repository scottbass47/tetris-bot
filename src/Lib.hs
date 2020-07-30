module Lib
  ( runBot
  ) where

import Board
import Tetromino
import Utils

runBot :: IO ()
runBot = readBoard "res/dt_canon.txt" >>= printBoard
