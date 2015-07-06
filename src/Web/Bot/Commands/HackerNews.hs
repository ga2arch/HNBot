{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module Web.Bot.Commands.HackerNews where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List
import Data.Time.Clock (getCurrentTime)
import Data.Maybe
import Database.Redis (connect, defaultConnectInfo)

import Web.Bot
import Web.HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

data Cache = Cache {
    cacheStories :: M.Map Int Story
,   cacheIds     :: [Int]
,   cacheAlreadySent :: M.Map User [Int]
}

topN n cache = do
    P.string $ "/top" ++ show n

    m <- lift $ getManager
    ids <- liftIO $ getTopN m n
    sendStories cache ids

threshold = do
    P.string $ "/threshold"

    send "Gimme a threshold: "

    next $ do
        t <- read <$> P.many1 P.digit
        when (t <= 20) $ do
            conn <- lift $ getDbConn
            User uid <- P.getState

            liftIO $ R.runRedis conn $ do
                R.set (C.pack $ show uid) (C.pack $ show t)
                return ()

            send "Ok"

ancor cache = do
    m <- getManager

    ids <- liftIO $ fmap fromJust $ getTopStories m
    isempty <- liftIO $ isEmptyMVar cache
    when isempty $ do
        c <- liftIO $ readMVar cache
        liftIO $ putMVar cache $ c { cacheIds = ids}

    forever $ do
        temp <- liftIO $ getTopStories m
        time <- liftIO $ getCurrentTime
        liftIO $ putStrLn $ "\n" ++ show time ++ "\n === Checking news ==="

        case temp of
            Just newTop -> do
                Cache {..} <- liftIO $ readMVar cache
                users <- getUsers

                sent <- foldM (process newTop cacheIds)
                    cacheAlreadySent users

                liftIO $ modifyMVar_ cache $ \c ->
                    return $ c {
                        cacheIds = newTop
                    ,   cacheAlreadySent = sent
                    }

            Nothing -> return ()

        liftIO $ threadDelay $ 60 * 1000 * 1000
        return ()
  where
    process newTop oldTop alreadySent (user, tbyte) = do
        let threshold = read $ C.unpack tbyte
            tOldTop = take threshold oldTop
            tNewTop = take threshold newTop

        let sent = if user `M.member` alreadySent
            then alreadySent M.! user
            else []

        let diff = (tNewTop \\ tOldTop) \\ sent
        liftIO $ putStrLn $ show user ++ " " ++ show diff
        sendStories' cache diff user
        return $ M.insert user (sent ++ diff) alreadySent

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
    u <- P.getState
    lift $ sendStories' cache ids u

sendStories' cache ids u = do
    m <- getManager

    mapM_ (\i -> do
        s <- getStory' m cache i
        sendStory s u) ids
  where
      sendStory (Just (Story sid title url)) u = do
          let msg = title ++ "\n\n" ++ url ++ "\n\n"
                    ++ "https://news.ycombinator.com/item?id=" ++ (show sid)

          send' msg u

      sendStory Nothing _ = return ()
