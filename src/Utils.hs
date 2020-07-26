module Utils
  (
    printBoard
  ) where

import Core 

printBoard :: Show a => Board a -> IO ()
printBoard = putStrLn . show
