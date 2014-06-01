module Server.Streams.Server where

import Server.Streams
import Server.Streams.Authenticate
import Server.Streams.Updates         ()
import Server.Streams.Requests        ()

serverStream :: Stream ()
serverStream = do

  sendMessage =<< authenticate

  choice [ requestStream
         , updateStream
         ]

requestStream :: Stream ()
requestStream = return ()

updateStream :: Stream ()
updateStream = return ()
