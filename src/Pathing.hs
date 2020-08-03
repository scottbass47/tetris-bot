module Pathing where

import Board
import Data.List (foldl')
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Debug.Trace
import SRS
import Tetromino hiding (Piece(L))
import Types

data Path =
  Path
    { inputs :: [Input]
    , start :: Tetromino
    , end :: Tetromino -- if terminalPosition board end -> add to paths and dont put back in queue else 
    }
  deriving (Show)

data Node =
  Node
    { backPointer :: Maybe Node
    , input :: Input
    , tetromino :: Tetromino
    }
  deriving (Show, Eq, Ord)

-- findPaths :: Board -> Tetromino -> [Path]
-- findPaths board tetromino = [Path result tetromino tetromino]
--   where
--     result = dfs board tetromino Set.empty
bfs ::
     Board
  -> Q.Seq Node
  -> S.Set Tetromino
  -> [Node]
  -> (Q.Seq Node, S.Set Tetromino, [Node])
bfs board toVisit visited endNodes
  | Q.null toVisit = (toVisit, visited, endNodes)
  | otherwise = bfs board (insertAll tail moves) visited' newEndNodes
  where
    head Q.:< tail = Q.viewl toVisit
    headT = tetromino head
    nextNodes = map (\i -> (i, tryPathMove headT i)) inputsToTry
    moves =
      map (uncurry $ Node (Just head)) .
      filter (flip S.notMember visited . snd) .
      fmap (fromJust . sequence) . filter ((/= Nothing) . snd) $
      nextNodes
    visited' = foldl' (flip S.insert) visited . map tetromino $ moves
    newEndNodes =
      if terminalPosition board headT
        then head : endNodes
        else endNodes
    --
    tryPathMove :: Tetromino -> Input -> Maybe Tetromino
    tryPathMove root (Move dir) =
      tryMove
        board
        root
        (case dir of
           L -> (-1, 0)
           R -> (1, 0))
    tryPathMove root (Rotate dir) = tryRotate board root dir
    tryPathMove root Nop = Nothing
    tryPathMove root ForceDown = tryMove board root (0, -1)
    --
    insertAll :: Q.Seq Node -> [Node] -> Q.Seq Node
    insertAll = foldl' (Q.|>)

inputsToTry :: [Input]
inputsToTry = [Move R, Move L, Rotate CW, Rotate CCW, ForceDown]
