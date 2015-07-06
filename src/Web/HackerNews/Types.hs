{-# LANGUAGE DeriveGeneric #-}

module Web.HackerNews.Types where

import Data.Aeson
import GHC.Generics

data Story = Story {
    id :: Int
,   title :: String
,   url :: String
} deriving (Show, Generic)

instance FromJSON Story
instance ToJSON   Story
