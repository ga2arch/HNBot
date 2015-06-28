{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleInstances, GeneralizedNewtypeDeriving #-}
module HackerNews where

import Data.Monoid
import Data.Default
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.ByteString.Lazy as BSL

newtype Ids a = Ids a deriving (Show, Generic)
type TopStories = Ids [Int]

instance Default TopStories where
    def =  Ids [0]

instance Functor Ids where
    fmap f (Ids ids) = Ids $ f ids

instance FromJSON TopStories
instance ToJSON   TopStories

data Story = Story {
    id :: Int
,   title :: String
,   url :: String
} deriving (Show, Generic)

instance FromJSON Story
instance ToJSON   Story

instance Default Story where
    def = Story 0 "" ""

makeReq m url = do
    let baseUrl = "https://hacker-news.firebaseio.com/v0/"
    req <- parseUrl $ baseUrl <> url

    withResponse req m $ \resp -> do
        body <- brConsume $ responseBody resp
        case (eitherDecode $ BSL.fromChunks body) of
            Left err -> return def
            Right d  -> return d

getStory :: Manager -> Int -> IO Story
getStory m sid = makeReq m $ "item/" <> (show sid) <> ".json"

getTopStories :: Manager -> IO [Int]
getTopStories m = fmap fromIds $ makeReq m "topstories.json"
  where
      fromIds (Ids ids) = ids

getTop10 :: Manager -> IO [Int]
getTop10 m = fmap (take 10) $ getTopStories m
