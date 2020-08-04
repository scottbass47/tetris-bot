module Evaluator where

import Board
import Types

newtype BoardEvaluator =
  BoardEvaluator
    { runEval :: Board -> Int
    }

mostEmptyRows :: BoardEvaluator
mostEmptyRows =
  BoardEvaluator $ length . filter id . map (all (not . isFilled)) . boardToList
