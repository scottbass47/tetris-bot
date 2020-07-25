{-# LANGUAGE InstanceSigs #-}

module Lib
    ( runBot
    ) where

data Board a = Board Int Int [[a]] 
data Mino = Filled | Empty  

instance Functor Board where
  fmap :: (a -> b) -> Board a -> Board b
  fmap f (Board r c board)= Board r c $ (fmap . fmap) f board 

instance Show Mino where
  show Filled = "*"
  show Empty = "-"

mkBoard :: Int -> Int -> a -> Board a
mkBoard r c val = Board r c (replicate r $ replicate c val)

mkBoardWithConstructor :: Int -> Int -> (Int -> Int -> a) -> Board a
mkBoardWithConstructor r c f = Board r c board  
  where board = [[f y x | x <- [0..(c-1)]] | y <- [0..(r-1)]] 

printBoard :: (Show a) => Board a -> IO ()
printBoard (Board _ _ board) = do
  sequence $ map (putStrLn . printRow) $ reverse board 
  return ()

printRow :: (Foldable f, Show a) => f a -> String
printRow = foldMap ((++ " ") . show)

runBot :: IO ()
runBot = printBoard $ mkBoardWithConstructor 20 width (\r c -> if (r * width + c + r) `mod` 2 == 0 then Filled else Empty) 
  where width = 10
