{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Main where

import Control.Concurrent.Async (async)
import Bot

import qualified Data.Map.Strict as M
import qualified Control.Monad.State as S
import qualified Database.Redis as R
import qualified HackerNews as HN

main = do
    conn <- R.connect R.defaultConnectInfo
    top10 <- HN.getTopStories
    async $ S.runStateT (ancor conn) (M.empty,top10)
    server conn  
