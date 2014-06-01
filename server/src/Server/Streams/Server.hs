module Server.Streams.Server where

import Server.Streams
import Server.Streams.Authenticate

serverStream :: Stream ()
serverStream = do

  sendMessage =<< authenticate
