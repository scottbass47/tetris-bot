module Pathing where

import Board
import Data.Maybe (fromJust)
import qualified Data.Sequence as Q
import qualified Data.Set as S
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

-- data Node = {
--     backPointer :: Maybe Node,
--     input :: Input,
--     tetromino :: Tetromino
--   }
-- findPaths :: Board -> Tetromino -> [Path]
-- findPaths board tetromino = [Path result tetromino tetromino]
--   where
--     result = dfs board tetromino Set.empty
-- [Nothing, Just ..., Nothing, Nothing]
-- [Just ...]
-- 
--dfs :: Board -> Tetromino -> Set.Set Tetromino -> [[(Input, Tetromino)]]
--dfs board root visited =
--  concatMap (\(i, t) -> map ((:) (i, root)) (dfs board t visited')) moves
--  where
--    visited' = Set.insert root visited
--    nextNodes = map (\i -> (i, tryPathMove i)) inputsToTry
--    moves =
--      filter (flip Set.notMember visited . snd) .
--      fmap (fromJust . sequence) . filter ((/= Nothing) . snd) $
--      nextNodes
--    -- allDfs = map (\input@(_, t) -> dfs board t visited')
--    --
--bfs ::
--     Board
--  -> Q.Seq Path
--  -> S.Set Tetromino
--  -> (Q.Seq Tetromino, S.Set Tetromino, [Path])
--bfs board toVisit visited
--  | Q.null toVisit = (toVisit, visited)
--  | otherwise = _
--  where
--    visited' = S.insert root visited
--    head Q.:< tail = Q.viewl toVisit
--    nextNodes = map (\i -> (i, tryPathMove i)) inputsToTry
--    moves =
--      filter (flip S.notMember visited . snd) .
--      fmap (fromJust . sequence) . filter ((/= Nothing) . snd) $
--      nextNodes
--    --
--    tryPathMove :: Tetromino -> Input -> Maybe Tetromino
--    tryPathMove root (Move dir) =
--      tryMove
--        board
--        root
--        (case dir of
--           L -> (-1, 0)
--           R -> (1, 0))
--    tryPathMove root (Rotate dir) = tryRotate board root dir
--    tryPathMove root Nop = Nothing
--    tryPathMove root ForceDown = tryMove board root (0, -1)
inputsToTry :: [Input]
inputsToTry = [Move R, Move L, Rotate CW, Rotate CCW, ForceDown]
