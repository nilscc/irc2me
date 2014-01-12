module Main where

import Test.Framework

import Test.IRC.IrcMessage
import Test.IRC.ProtoBuf
import Test.IRC.TLS

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = ircmsg_tests
     ++ protobuf_tests
     ++ tls_tests
