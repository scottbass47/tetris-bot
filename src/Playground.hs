module Playground where

import Tetromino
import Utils

-- Boards
miniIO = readBoard "res/mini.txt"

canonIO = readBoard "res/dt_canon.txt"

tPiece = mkTetromino T
