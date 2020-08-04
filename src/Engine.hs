module Engine where

import Board
import Data.List
import Evaluator
import Pathing
import Tetromino
import Types

makeMove :: Board -> Tetromino -> BoardEvaluator -> Maybe Path
makeMove board tetromino eval = fst <$> foldl' folder Nothing allPaths
  where
    allPaths = findPaths board tetromino
    --
    folder :: (Maybe (Path, Int) -> Path -> Maybe (Path, Int))
    folder Nothing p = Just (p, pathScore p)
    folder acc@(Just (bestPath, bestScore)) path =
      let score = pathScore path
       in if score > bestScore
            then Just (path, score)
            else acc
    --
    pathScore :: Path -> Int
    pathScore = runEval eval . pathBoard
