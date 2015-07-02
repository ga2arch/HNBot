{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List.Split (splitOn)
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import qualified Text.Parsec as P

helpCmd = do
    P.string "/help"

    send "/* to multiply two numbers"

mulCmd = do
    P.string "/*"

    send "First number: "

    next $ do
        n1 <- read <$> P.many1 P.digit

        send "Second number: "

        next $ do
            n2 <- read <$> P.many1 P.digit
            send $ show $ n1 * n2

main = do
    conn <- connect defaultConnectInfo
    token <- fmap (head . splitOn "\n") $ readFile "token"

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        runBot config $ do
            mapM_ (addCmd . Cmd) [helpCmd, mulCmd]
            server
            handler "first"
            handler "second"

    forever $ threadDelay maxBound
    return ()
