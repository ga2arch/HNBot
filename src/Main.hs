{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Text.ParserCombinators.Parsec.Token
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import Data.List.Split (splitOn)

import qualified Text.Parsec as P

helpCmd = do
    _ <- P.string "/help"

    u <- P.getState
    lift $ send "/*2 to multiply a number" u

per2Cmd = do
    u <- P.getState
    _ <- P.string "/*2"

    lift $ send "Scrivi il numero da multiplicare per 2:" u

    lift $ setCont u $ do
        d <- read <$> P.many1 P.digit
        lift $ send (show $ d * 2) u

main = do
    conn <- connect defaultConnectInfo
    manager <- newManager tlsManagerSettings
    token <- fmap (head . splitOn "\n") $ readFile "token"

    let config = BotConfig 8080 token manager conn
    runBot config $ do
        mapM_ (addCmd . Cmd) [helpCmd, per2Cmd]
        server
        handler

    return ()
