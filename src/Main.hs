{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import Data.List.Split (splitOn)

import qualified Text.Parsec as P

echoCmd = do
    _ <- P.char '/'
    d <- P.many P.anyToken

    u <- P.getState
    lift $ send d u

main = do
    conn <- connect defaultConnectInfo
    manager <- newManager tlsManagerSettings
    token <- fmap (head . splitOn "\n") $ readFile "token"

    let config = BotConfig 8080 token manager conn
    runBot config $ do
        addCmd $ Cmd echoCmd
        server
        handler


    return ()
