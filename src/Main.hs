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
import Data.Time.Clock (getCurrentTime)
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Process
import System.Directory
import System.Random (randomRIO)

import Bot
import HackerNews

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

data Cache = Cache {
    cacheStories :: M.Map Int Story
,   cacheIds     :: [Int]
,   cacheAlreadySent :: M.Map User [Int]
}

helpCmd = do
    P.string "/help"

    let msg = ["/top3", "/top5", "/top10", "/threshold - Sets the news threshold"]

    send $ mconcat $ intersperse "\n" msg

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

downloadAndSend name url = do
    (o, p) <- liftIO $ do
        o <- getCurrentDirectory
        p <- makeAbsolute "static/"
        setCurrentDirectory p

        return (o, p)

    res <- liftIO $ try $
            readProcess "youtube-dl" ["-x", "--audio-format", "mp3", url] []

    liftIO $ setCurrentDirectory o

    case res of
        Left (e :: SomeException) -> do
            liftIO $ print e
            send "There was an error"

        Right _ -> do
            liftIO $ do
                setCurrentDirectory o
                async $ do
                    threadDelay $ 60 * 1000 * 1000
                    removeFile $ p ++ name

            path <- liftIO $ makeAbsolute $ "static/" ++ name
            sendDoc path
            send $ "http://dogetu.be:8080/static/" ++ urlEncode name

getYUrlFilename url =
    liftIO $ try $ do
        t <- fmap init $ readProcess "youtube-dl"
            ["-x", "--get-filename", url] []
        return $ if endswith ".webm" t
            then replace ".webm" ".mp3" t
            else replace ".m4a"  ".mp3" t

bombz = do
    P.string "/bombz"

    songs <- liftIO $ fmap read $ readFile "songs"
    n <- liftIO $ randomRIO (0, (length songs - 1))
    let url = songs !! n

    t <- getYUrlFilename url
    case t of
        Right name -> do
            send $ "Fetching " ++ name
            downloadAndSend name url
        Left (e :: SomeException) -> send "There was an error"

youtubeDl = do
    P.manyTill P.anyToken (P.try $ P.string "youtu")
    (P.try $ P.string ".be/") P.<|> (P.try $ P.string "be.com/watch?v=")

    rest  <- P.many1 P.anyToken

    let url = "https://www.youtube.com/watch?v=" ++ rest
    liftIO $ print url

    n <- getYUrlFilename url

    case n of
        Right name -> do
            send $ "Fetching " ++ name
            downloadAndSend name url

        Left (e :: SomeException) -> do
            liftIO $ print e
            send "There was an error"

ancor cache = do
    m <- getManager

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

main = do
    conn <- connect defaultConnectInfo
    token <- fmap init $ readFile "token"

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        ids <- fmap fromJust $ getTopStories manager
        cache <- newMVar $ Cache M.empty ids M.empty

        runBot config $ do
            addCmd helpCmd
            addCmd $ topN 3 cache
            addCmd $ topN 5 cache
            addCmd $ topN 10 cache
            addCmd threshold
            addCmd youtubeDl
            addCmd bombz

            server
            runAsync $ handler "first"
            runAsync $ handler "second"

            ancor cache

    --forever $ threadDelay maxBound
    return ()
