module Utils
  (
    printBoard
  ) where

import Board 

printBoard :: Show a => Board a -> IO ()
printBoard = putStrLn . show
