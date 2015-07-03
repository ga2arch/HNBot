{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards,
             FlexibleContexts #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot
import HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

data Cache = Cache {
    cacheStories :: M.Map Int Story
,   cacheIds     :: [Int]
,   cacheAlreadySent :: [Int]
}

helpCmd = do
    P.string "/help"
    send "/top3  get the top 3"
    send "/top5  get the top 5"
    send "/top10 get the top 10"

topN n cache = do
    P.string $ "/top" ++ show n

    m <- lift $ getManager
    ids <- liftIO $ getTopN m n
    sendStories cache ids

threshold = do
    P.string $ "/threshold"

    send "Threshold: "

    next $ do
        t <- read <$> P.many1 P.digit
        when (t <= 20) $ do
            conn <- lift $ getDbConn
            User uid <- P.getState

            liftIO $ R.runRedis conn $ do
                Right u <- R.get $ C.pack . show $ uid
                case u of
                    Just _ ->
                        R.set (C.pack $ show uid) (C.pack $ show t)
                        >> return ()
                    Nothing -> return ()

ancor cache = do
    m <- getManager
    users <- getUsers

    forever $ do
        Cache {..} <- liftIO $ readMVar cache
        temp <- liftIO $ getTopStories m

        case temp of
            Just newTop ->
                forM_ users $ \(user, tbyte) -> do
                    let threshold = read $ C.unpack tbyte
                    let tOldTop = take threshold cacheIds
                    let tNewTop = take threshold newTop

                    let diff = (tNewTop \\ tOldTop) \\ cacheAlreadySent
                    mapM_ (\i -> do
                        s <- getStory' m cache i
                        (flip send') user . title . fromJust $ s) diff

                    liftIO $ modifyMVar_ cache $ \c ->
                        return $ c {
                            cacheIds = newTop
                        ,   cacheAlreadySent = cacheAlreadySent ++ diff
                        }

            Nothing -> return ()
        liftIO $ threadDelay $ 60 * 1000 * 1000
        return ()

getStory' m cache sid =
    liftIO $ modifyMVar cache $ \c@(Cache {..}) -> do
        if sid `M.member` cacheStories
            then return (c, Just $ cacheStories M.! sid)
            else do
                story <- getStory m sid
                case story of
                    Just s -> do
                        let nStories = M.insert sid s cacheStories
                        return (c { cacheStories = nStories }, story)
                    Nothing -> return (c, Nothing)

sendStories cache ids = do
    m <- lift $ getManager
    mapM_ (\i -> do
        s <- getStory' m cache i
        sendStory s) ids
  where
      sendStory = send . title . fromJust

main = do
    conn <- connect defaultConnectInfo
    token <- fmap (head . splitOn "\n") $ readFile "token"

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        ids <- fmap fromJust $ getTopStories manager
        cache <- newMVar $ Cache M.empty ids []

        runBot config $ do
            addCmd helpCmd
            addCmd $ topN 3 cache
            addCmd $ topN 5 cache
            addCmd $ topN 10 cache
            addCmd $ threshold

            server
            handler "first"
            handler "second"

            runAsync $ ancor cache

    forever $ threadDelay maxBound
    return ()
