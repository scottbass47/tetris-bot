module Main where

import Lib

data Board a = Board Int Int [[a]] 

mkBoard :: Int -> Int -> a -> Board a
mkBoard r c val = Board r c  (replicate r $ replicate c val)

main :: IO ()
main = runBot
