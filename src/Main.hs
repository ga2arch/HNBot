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

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

helpCmd :: Parser
helpCmd = do
    P.string "/help"

    let msg = ["/top3", "/top5", "/top10", "/threshold - Sets the news threshold"]

    send $ mconcat $ intersperse "\n" msg

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
            path <- liftIO $ do
                setCurrentDirectory o
                async $ do
                    threadDelay $ 60 * 1000 * 1000
                    removeFile $ p ++ name
                makeAbsolute $ "static/" ++ name

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

main = do
    conn <- connect defaultConnectInfo
    token <- fmap init $ readFile "token"

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        cache <- newMVar $ Cache M.empty [] M.empty

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
