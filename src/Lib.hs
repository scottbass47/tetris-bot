
module Lib
    ( runBot
    ) where

import Core
import Utils

runBot :: IO ()
runBot = do
  let width = 10
  let board = mkBoardWithConstructor 20 width (\(r, c) -> Mino ((r * width + c + r) `mod` 2 == 0)) 
  printBoard board 
