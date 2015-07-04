{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleInstances, GeneralizedNewtypeDeriving #-}
module HackerNews where

import Control.Exception
import Data.Monoid
import Data.Maybe
import Data.Default
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.ByteString.Lazy as BSL

data Story = Story {
    id :: Int
,   title :: String
,   url :: String
} deriving (Show, Generic)

instance FromJSON Story
instance ToJSON   Story

makeReq m url = do
    let baseUrl = "https://hacker-news.firebaseio.com/v0/"
    req <- parseUrl $ baseUrl <> url

    resp <- try $ httpLbs req m
    case resp of
        Right body -> case (eitherDecode $ responseBody body) of
            Left e -> print e >> return Nothing
            Right d  -> return $ Just d
        Left (e :: SomeException) -> print e >> return Nothing

getStory :: Manager -> Int -> IO (Maybe Story)
getStory m sid = makeReq m $ "item/" <> (show sid) <> ".json"

getTopStories :: Manager -> IO (Maybe [Int])
getTopStories m = makeReq m "topstories.json"

getTopN :: Manager -> Int -> IO [Int]
getTopN m n = fmap (take n . fromJust) $ getTopStories m
