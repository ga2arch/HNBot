{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards,
             FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Control.Concurrent.MVar (newMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment
import System.Posix.Signals

import Web.Bot
import Web.Bot.Commands
import Web.HackerNews

import qualified Data.Acid as A
import qualified Data.Map as M

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    token <- liftIO $ getEnv "TOKEN"
    let config = BotConfig 8443 token manager

    ids <- fmap fromJust $ getTopStories manager
    db <- liftIO $ A.openLocalStateFrom "users/" (Database [])
    cache <- newMVar $ Cache M.empty ids M.empty

    installHandler sigTERM (Catch $ A.closeAcidState db) Nothing

    runBot config $ do
        addCmd help
        addCmd $ topN 3 cache
        addCmd $ topN 5 cache
        addCmd $ topN 10 cache
        addCmd $ threshold db
        addCmd youtube
        addCmd bombz
        addCmd reminder

        runAsync $ handler "first"
        runAsync $ handler "second"

        ancor cache db

    return ()
