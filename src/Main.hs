{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Main where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.List.Split (splitOn)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import qualified Data.Map.Strict as M
import qualified Control.Monad.State as S
import qualified Database.Redis as R
import qualified HackerNews as HN

main = do
    manager <- newManager tlsManagerSettings
    token <- fmap (head . splitOn "\n") $ readFile "token"

    conn <- R.connect R.defaultConnectInfo
    top <- HN.getTopStories manager    

    chan <- newChan
    state <- newMVar $ BotState { botNewsSent = M.empty, botTop = top }

    let config = BotData { redisConn = conn,
        botPort = 8080, botState = state,
        botToken = token, botChan = chan, botManager = manager }

    runBot config $ do
        ancor
        handler
        server

    closeManager manager
