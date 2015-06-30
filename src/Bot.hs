{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, GeneralizedNewtypeDeriving,
             RankNTypes, ExistentialQuantification #-}
module Bot where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Maybe (fromJust)
import GHC.Generics
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as C

data User = User {
    id :: Int
} deriving (Show, Generic)

instance FromJSON User
instance ToJSON   User

data Message = Message {
    message_id :: Int
,   from :: User
,   text :: Maybe String
} deriving (Show, Generic)

instance FromJSON Message
instance ToJSON   Message

data Update = Update {
    update_id :: Int
,   message :: Maybe Message
} deriving (Show, Generic)

instance FromJSON Update
instance ToJSON   Update

newtype Bot a = B { unBot :: ReaderT BotData IO a }
    deriving (Functor, Applicative, Monad, MonadReader BotData,
              MonadIO, MonadCatch, MonadThrow, MonadMask)

data BotData = BotData BotConfig BotState

data BotConfig = BotConfig {
    botPort    :: Int
,   botToken   :: String
,   botManager :: Manager
,   botDbConn  :: R.Connection
}

data BotState = forall a. BotState {
    botCache :: MVar a
}

runBot :: BotData -> Bot a -> IO a
runBot bdata f = runReaderT (unBot f) bdata

getPort :: Bot Int
getPort = asks $ \(BotData c _) -> botPort c

getToken :: Bot String
getToken = asks $ \(BotData c _) -> botToken c

getManager :: Bot Manager
getManager = asks $ \(BotData c _) -> botManager c

getDbConn :: Bot R.Connection
getDbConn = asks $ \(BotData c _) -> botDbConn c

getUsers :: Bot [(User, C.ByteString)]
getUsers = do
    conn <- getDbConn
    users <- liftIO $ R.runRedis conn $ do
        ks <- query
        mapM (\k -> do
            Right t <- R.get k
            return (User . toInt $ k,
                    fromJust t)) ks
    return users
  where
    query = do
        Right ks <- R.keys "*"
        return ks

    toInt :: C.ByteString -> Int
    toInt = read . C.unpack

withCache :: (forall a. a -> Bot (a, b)) -> Bot b
withCache f = do
    d <- ask
    withCache' d f
  where
      withCache' :: BotData -> (forall a. a -> Bot (a, b)) -> Bot b
      withCache' (BotData _ (BotState m)) f =
          mask $ \restore -> do
              cache <- liftIO $ takeMVar m
              (newCache, result) <- restore (f cache)
                `onException` (liftIO $ putMVar m cache)
              liftIO $ putMVar m newCache
              return result

send :: String -> User -> Bot ()
send text u@(User uid) = do
    m     <- getManager
    token <- getToken

    let baseUrl = "https://api.telegram.org/bot"
    let payload = urlEncodeVars [("chat_id", (show uid)),
                                ("text", text),
                                ("disable_web_page_preview", "true")]
    let url = mconcat [baseUrl, token, "/sendMessage?", payload]

    req <- parseUrl url
    r <- liftIO $ try $ httpLbs req m
    case r of
        Left (ex :: SomeException) -> liftIO $ print ex
        Right _  -> return ()

test :: Bot ()
test = do
    c <- withCache $ \cache -> return (cache, "ciao")
    liftIO $ print c
