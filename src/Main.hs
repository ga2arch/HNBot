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

    u <- P.getState
    lift $ send "/*2 to multiply a number" u

per2Cmd = do
    u <- P.getState
    P.string "/+"

    lift $ send "Scrivi il primo n:" u

    lift $ addCont u $ do
        n1 <- read <$> P.many1 P.digit

        lift $ send "Scrivi il secondo n:" u
        
        lift $ addCont u $ do
            n2 <- read <$> P.many1 P.digit
            lift $ send (show $ n1 * n2) u

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
