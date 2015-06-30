{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, GeneralizedNewtypeDeriving,
             RankNTypes, ExistentialQuantification #-}
module Bot where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype Bot a = B { unBot :: ReaderT BotData IO a }
    deriving (Functor, Applicative, Monad, MonadReader BotData,
              MonadIO, MonadCatch, MonadThrow)

data BotData = BotData BotConfig BotState

data BotConfig = BotConfig {
    botPort    :: Int
,   botToken   :: Text
,   botManager :: Manager
}

data BotState = forall a. (Show a) => BotState {
    botCache :: MVar a
}

runBot :: BotData -> Bot a -> IO a
runBot bdata f = runReaderT (unBot f) bdata

getPort :: Bot Int
getPort = asks $ \(BotData c _) -> botPort c

getToken :: Bot Text
getToken = asks $ \(BotData c _) -> botToken c

getManager :: Bot Manager
getManager = asks $ \(BotData c _) -> botManager c

withCache :: (forall a. a -> Bot (a, b)) -> Bot b
withCache f = do
    d <- ask
    withCache' d f
  where
      withCache' :: BotData -> (forall a. a -> Bot (a, b)) -> Bot b
      withCache' (BotData _ (BotState c)) f = do
          cache <- liftIO $ takeMVar c
          (newCache, result) <- f cache
          liftIO $ putMVar c newCache
          return result

test :: Bot ()
test = do
    c <- withCache $ \cache -> return (cache, "ciao")
    liftIO $ print c
