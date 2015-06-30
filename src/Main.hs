{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Main where

import Control.Concurrent.MVar
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Bot

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- data T = forall a. Show a => T {
--     d :: [a]
-- }
--
-- instance Show T where
--     show (T a) = show a

main = do
    manager <- newManager tlsManagerSettings
    token <- fmap (head . T.splitOn "\n") $ TIO.readFile "token"

    s <- newMVar [1]
    runBot (BotData (BotConfig 8080 token manager) (BotState s)) test

    return ()
