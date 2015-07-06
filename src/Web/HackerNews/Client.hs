{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Web.HackerNews.Client where

import Control.Exception
import Data.Monoid
import Data.Maybe
import Data.Default
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Web.HackerNews.Types
import qualified Data.ByteString.Lazy as BSL

makeReq m url = do
    let baseUrl = "https://hacker-news.firebaseio.com/v0/"
    req <- parseUrl $ baseUrl <> url

    resp <- try $ httpLbs req m
    case resp of
        Right body -> case (eitherDecode $ responseBody body) of
            Left e -> print e >> return Nothing
            Right d  -> return $ Just d
        Left (e :: SomeException) -> print e >> return Nothing
