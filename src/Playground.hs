module Playground where

import Board
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Pathing
import Tetromino
import Types
import Utils

-- Boards
miniIO = readBoard "res/mini.txt"

canonIO = readBoard "res/dt_canon.txt"

tPiece = mkTetromino T

-- BFS
runBfs :: IO ()
runBfs = do
  mini <- canonIO
  let t = moveTetromino (5, 15) tPiece
  let startNode = Node Nothing Nop t
  let (_, _, nodes) = bfs mini (Q.singleton startNode) (S.singleton t) []
  sequence_ . map (putStrLn . show . flip placeTetromino mini . tetromino) $
    nodes
