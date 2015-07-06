{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards,
             FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List
import Data.String.Utils (replace, endswith)
import Data.Maybe
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Process
import System.Directory
import System.Random (randomRIO)

import Web.Bot
import Web.Bot.Commands
import Web.HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

main = do
    conn <- connect defaultConnectInfo
    token <- fmap init $ readFile "token"

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        ids <- fmap fromJust $ getTopStories manager
        cache <- newMVar $ Cache M.empty ids M.empty

        runBot config $ do
            addCmd help
            addCmd $ topN 3 cache
            addCmd $ topN 5 cache
            addCmd $ topN 10 cache
            addCmd threshold
            addCmd youtube
            addCmd bombz

            server
            runAsync $ handler "first"
            runAsync $ handler "second"

            ancor cache

    --forever $ threadDelay maxBound
    return ()
