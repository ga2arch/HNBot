{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Web.Bot.Commands.HackerNews
        ( topN
        , threshold
        , ancor
        , Cache (..)
        , Database (..)
        ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List
import Data.Time.Clock (getCurrentTime)
import Data.Maybe
import Data.SafeCopy

import Web.Bot
import Web.HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Data.Acid as A

data Database = Database [(User, Int)]
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Database)

addUser :: User -> Int -> A.Update Database ()
addUser user t = do
    Database users <- get
    put $ Database ((user, t) : users)

viewUsers :: A.Query Database [(User, Int)]
viewUsers = do
    Database users <- ask
    return users

$(A.makeAcidic ''Database ['addUser, 'viewUsers])

data Cache = Cache {
    cacheStories     :: M.Map Int Story
,   cacheIds         :: [Int]
,   cacheAlreadySent :: M.Map User [Int]
}

getUsers db = liftIO $ A.query db ViewUsers

topN n cache = do
    P.string $ "/top" ++ show n

    m <- lift $ getManager
    ids <- liftIO $ getTopN m n
    sendStories cache ids

threshold db = do
    P.string "/threshold"

    send "Gimme a threshold: "

    next $ do
        t <- read <$> P.many1 P.digit
        when (t <= 20) $ do
            user <- P.getState

            liftIO $ A.update db (AddUser user t)

            send "Ok"

ancor cache db = do
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
                users <- getUsers db

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
    process newTop oldTop alreadySent (user, threshold) = do
        let tOldTop = take threshold oldTop
            tNewTop = take threshold newTop
            sent    = M.findWithDefault [] user alreadySent
            diff    = (tNewTop \\ tOldTop) \\ sent

        liftIO $ putStrLn $ show user ++ " " ++ show diff
        sendStories' cache diff user
        return $ M.insert user (sent ++ diff) alreadySent

getStory' m cache sid =
    liftIO $ modifyMVar cache $ \c@(Cache {..}) -> do
        maybe (fetch c m sid)
              (return . (,) c . Just) $ M.lookup sid cacheStories
  where
    fetch c m sid = do
        story <- getStory m sid
        case story of
            Just s -> let nStories = M.insert sid s (cacheStories c)
                      in  return (c { cacheStories = nStories }, story)
            Nothing -> return (c, Nothing)

sendStories cache ids = do
    u <- P.getState
    lift $ sendStories' cache ids u

sendStories' cache ids u = do
    m <- getManager

    forM_ ids $ \i -> (flip sendStory) u =<< (getStory' m cache i)
  where
      sendStory (Just (Story sid title url)) u = do
          let msg = title ++ "\n\n" ++ url ++ "\n\n"
                    ++ "https://news.ycombinator.com/item?id=" ++ (show sid)

          send' msg u

      sendStory Nothing _ = return ()
