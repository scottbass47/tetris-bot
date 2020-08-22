module Lib
  ( runBot,
  )
where

import Sockets

runBot :: IO ()
runBot = runServer
