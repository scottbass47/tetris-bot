
module Lib
    ( runBot
    ) where

import Board
import Utils
import Tetromino

runBot :: IO ()
runBot = do
  let width = 10
  let board = mkBoard' 20 width (\(r, c) -> Mino ((r * width + c + r) `mod` 2 == 0)) 
  printBoard $ clearRows [0,1,2] board 
