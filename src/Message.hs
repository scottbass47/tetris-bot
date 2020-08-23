{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics
import Tetromino
import Types

data Msg
  = Initial InitialD
  | MakeMove MakeMoveD
  | Spawn SpawnD
  deriving (Show, Eq)

data MsgT = InitialT | MakeMoveT | SpawnT deriving (Show, Eq)

data InitialD = InitialD
  { boardSize :: Point
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MakeMoveD = MakeMoveD
  { inputs :: [MoveVec]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SpawnD = SpawnD
  { board :: [Point],
    pieceType :: Piece
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MoveVec = MoveVec
  { input :: Input,
    position :: Point,
    orientation :: Orientation
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJSON MsgT where
  parseJSON = withText "Message Type" $ \s ->
    case s of
      "initial" -> return InitialT
      "makeMove" -> return MakeMoveT
      "spawn" -> return SpawnT
      _ -> fail "wrong again"

instance ToJSON MsgT where
  toJSON InitialT = "initial"
  toJSON MakeMoveT = "makeMove"
  toJSON SpawnT = "spawn"

instance FromJSON Msg where
  parseJSON = withObject "Message" $ \obj -> do
    msgType <- obj .: "type"
    case msgType of
      InitialT -> Initial <$> obj .: "data"
      SpawnT -> Spawn <$> obj .: "data"
      MakeMoveT -> MakeMove <$> obj .: "data"

instance ToJSON Msg where
  toJSON (Initial d) = object ["type" .= InitialT, "data" .= d]
  toJSON (Spawn d) = object ["type" .= SpawnT, "data" .= d]
  toJSON (MakeMove d) = object ["type" .= MakeMoveT, "data" .= d]

