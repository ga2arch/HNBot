module Web.HackerNews
    ( getStory
    , getTopStories
    , getTopN
    , Story (..)
    ) where

import Data.Maybe
import Data.Monoid
import Network.HTTP.Client

import Web.HackerNews.Types
import Web.HackerNews.Client

getStory :: Manager -> Int -> IO (Maybe Story)
getStory m sid = makeReq m $ "item/" <> (show sid) <> ".json"

getTopStories :: Manager -> IO (Maybe [Int])
getTopStories m = makeReq m "topstories.json"

getTopN :: Manager -> Int -> IO [Int]
getTopN m n = fmap (take n . fromJust) $ getTopStories m
