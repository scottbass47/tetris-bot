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

runFindPaths :: IO ()
runFindPaths = do
  board <- canonIO
  let t = moveTetromino (5, 15) tPiece
  let paths = findPaths board t
  let printBoard' = putStrLn . show . flip placeTetromino board . last
  let printInputs = putStrLn . show
  let printPath (Path inputs nodes) = printBoard' nodes >> printInputs inputs
  sequence_ . map printPath $ paths
