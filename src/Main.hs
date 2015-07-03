{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List.Split (splitOn)
import Data.Maybe
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot
import HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P

data Cache = Cache {
    cacheStories :: M.Map Int Story
}

helpCmd = do
    P.string "/help"
    send "/top3 get the top 3"

topN n cache = do
    P.string $ "/top" ++ show n

    m <- lift $ getManager
    ids <- liftIO $ getTopN m n
    mapM_ ((>>= sendStory) . getStory' m cache) ids
    
  where
    getStory' m cache sid =
        liftIO $ modifyMVar cache $ \(Cache stories) -> do
            if sid `M.member` stories
                then return (Cache stories, Just $ stories M.! sid)
                else do
                    story <- getStory m sid
                    case story of
                        Just s -> do
                            let nStories = M.insert sid s stories
                            return (Cache nStories, story)
                        Nothing -> return (Cache stories, Nothing)

    sendStory = send . title . fromJust

main = do
    conn <- connect defaultConnectInfo
    token <- fmap (head . splitOn "\n") $ readFile "token"
    cache <- newMVar $ Cache M.empty

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        runBot config $ do
            addCmd helpCmd
            addCmd $ topN 3 cache
            addCmd $ topN 5 cache
            addCmd $ topN 10 cache


            server
            handler "first"
            handler "second"

    forever $ threadDelay maxBound
    return ()
