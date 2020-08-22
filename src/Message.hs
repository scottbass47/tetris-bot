{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson
import Data.Aeson.Types
import Tetromino
import Types

data Msg
  = Initial InitialD
  | MakeMove MakeMoveD
  | Spawn SpawnD
  deriving (Show, Eq)

data MsgT = InitialT | MakeMoveT | SpawnT deriving (Show, Eq)

data InitialD = InitialD
  { rows :: Int,
    cols :: Int
  }
  deriving (Show, Eq)

data MakeMoveD = MakeMoveD
  { inputs :: [MoveVec]
  }
  deriving (Show, Eq)

data SpawnD = SpawnD
  { board :: [Point],
    pieceType :: Piece
  }
  deriving (Show, Eq)

data MoveVec = MoveVec
  { input :: Input,
    position :: Point,
    orientation :: Orientation
  }
  deriving (Show, Eq)

instance FromJSON InitialD where
  parseJSON = withObject "Board Size" $ \obj -> InitialD <$> obj .: "rows" <*> obj .: "cols"

instance ToJSON InitialD where
  toJSON (InitialD r c) = object ["rows" .= c, "cols" .= c]

instance FromJSON MsgT where
  parseJSON (String s)
    | s == "initial" = return InitialT
    | s == "makeMove" = return MakeMoveT
    | s == "spawn" = return SpawnT
    | otherwise = error "wrong again"
  parseJSON _ = error "wrong"

instance FromJSON Msg where
  parseJSON = withObject "Message" $ \obj -> do
    msgType <- obj .: "type"
    case msgType of
      InitialT -> Initial <$> obj .: "data"
      _ -> error "not implemented"
