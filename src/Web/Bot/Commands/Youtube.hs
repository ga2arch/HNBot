{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts,
             ScopedTypeVariables #-}
module Web.Bot.Commands.Youtube
        ( youtube
        , bombz) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Network.HTTP.Base (urlEncode)
import System.Process
import System.Directory
import System.Random (randomRIO)
import System.FilePath.Posix

import Web.Bot
import Web.HackerNews

import qualified Text.Parsec as P

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
        return $ replaceExtension t ".mp3"

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

youtube = do
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
