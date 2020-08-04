module Playground where

import Board
import Data.Maybe
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Engine
import Evaluator
import Pathing
import Tetromino
import Types
import Utils

-- Boards
miniIO = readBoard "res/mini.txt"

canonIO = readBoard "res/dt_canon.txt"

tPiece = mkTetromino T

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
  sequence_ . map printPath $ paths

runEngine :: IO ()
runEngine = do
  board <- canonIO
  let t = moveTetromino (5, 15) tPiece
  let path = fromJust $ makeMove board t mostEmptyRows
  printPath $ fromJust $ makeMove (pathBoard path) t mostEmptyRows

printPath (Path inputs nodes pathBoard) =
  printShow pathBoard >> printShow inputs

printShow :: (Show a) => a -> IO ()
printShow = putStrLn . show
