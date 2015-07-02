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
    P.string "/help"

    send "/*2 to multiply a number"

per2Cmd = do
    P.string "/+"

    send "Scrivi il primo n:"

    next $ do
        n1 <- read <$> P.many1 P.digit

        send "Scrivi il secondo n:"

        next $ do
            n2 <- read <$> P.many1 P.digit
            send $ show $ n1 * n2

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
