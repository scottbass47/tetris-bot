{-# LANGUAGE OverloadedStrings #-}

module Sockets where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Message
import qualified Network.WebSockets as WS

type Client = WS.Connection

type ServerState = Maybe Client

newServerState :: ServerState
newServerState = Nothing

clientExists :: ServerState -> Bool
clientExists = not . isNothing

addClient :: Client -> ServerState -> ServerState
addClient client _ = Just client

removeClient :: Client -> ServerState -> ServerState
removeClient client _ = Nothing

runServer :: IO ()
runServer = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn :: IO LBS.ByteString
    serverState <- readMVar state
    case decode' msg of
      Just (Initial d) ->
        if clientExists serverState
          then
            WS.sendTextData conn alreadyConnected
              >> T.putStrLn alreadyConnected
          else do
            putStrLn . show . boardSize $ d
            flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                let s' = addClient conn s
                putStrLn "Client connected"
                return s'
              talk conn state
        where
          alreadyConnected = "Client already connected" :: Text
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient conn s in return (s', s')
            return s
      _ -> WS.sendTextData conn ("Invalid handshake format" :: Text)

talk :: Client -> MVar ServerState -> IO ()
talk conn state = forever $ do
  msg <- WS.receiveData conn :: IO LBS.ByteString
  case decode' msg of
    Just (Spawn d) -> do
      putStrLn $ "Board: " <> (show . board $ d)
      putStrLn $ "Piece: " <> (show . pieceType $ d)
    _ -> WS.sendTextData conn $ "Invalid message: " <> msg

-- T.putStrLn $ "Received: " <> msg
