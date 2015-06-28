{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Main where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar

import Bot

import qualified Data.Map.Strict as M
import qualified Control.Monad.State as S
import qualified Database.Redis as R
import qualified HackerNews as HN

main = do
    conn <- R.connect R.defaultConnectInfo
    top <- HN.getTopStories

    state <- newMVar $ BotState { botNewsSent = M.empty, botTop = top }
    let config = BotConfig { redisConn = conn,
        botPort = 8080, botState = state, botToken = "" }

    runBot config $ do
        ancor
        server
