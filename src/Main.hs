{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent.MVar
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import Data.List.Split (splitOn)

-- data T = forall a. Show a => T {
--     d :: [a]
-- }
--
-- instance Show T where
--     show (T a) = show a

main = do
    conn <- connect defaultConnectInfo
    manager <- newManager tlsManagerSettings
    token <- fmap (head . splitOn "\n") $ readFile "token"

    s <- newMVar [1]
    runBot (BotData (BotConfig 8080 token manager conn) (BotState s)) test

    return ()
