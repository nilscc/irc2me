{-# LANGUAGE RankNTypes #-}

module Database.Types where

import Database.HDBC

type Query a = IConnection con => con -> a
