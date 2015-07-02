{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List.Split (splitOn)
import Database.Redis (connect, defaultConnectInfo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import qualified Text.Parsec as P

data C = C {
    cData :: Int
}

helpCmd = do
    P.string "/help"

    send "/* to multiply two numbers"

mulCmd m = do
    P.string "/*"

    send "First number: "

    next $ do
        n1 <- read <$> P.many1 P.digit

        send "Second number: "

        next $ do
            n2 <- read <$> P.many1 P.digit
            send $ show $ n1 * n2

            liftIO $ modifyMVar_ m $ \(C d) ->
                return $ C (d + n1*n2)

getSumCmd m = do
    P.string "/getSum"

    C s <- liftIO $ readMVar m

    send $ show s

main = do
    conn <- connect defaultConnectInfo
    token <- fmap (head . splitOn "\n") $ readFile "token"
    m <- newMVar $ C 0

    withManager tlsManagerSettings $ \manager -> do
        let config = BotConfig 8080 token manager conn

        runBot config $ do
            addCmd helpCmd
            addCmd $ mulCmd m
            addCmd $ getSumCmd m

            server
            handler "first"
            handler "second"

    forever $ threadDelay maxBound
    return ()
