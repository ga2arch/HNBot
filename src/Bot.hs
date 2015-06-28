{-# LANGUAGE OverloadedStrings, DeriveGeneric,
             FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Bot where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Maybe (fromJust)
import Data.List
import Data.List.Split (splitOn)
import GHC.Generics
import Web.Scotty
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.Map.Strict as M
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as Re

import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R
import qualified HackerNews as HN

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

type NewsSent = M.Map Int [Int]

data BotConfig = BotConfig {
    redisConn :: R.Connection
,   botPort   :: Int
,   botToken  :: String
,   botState  :: MVar BotState
}

data BotState = BotState {
    botNewsSent :: NewsSent
,   botTop      :: [Int]
} deriving (Show)

newtype Bot a = Bot { unBot :: Re.ReaderT BotConfig IO a }
    deriving (Monad, Applicative, Functor,
              Re.MonadIO, Re.MonadReader BotConfig)

runBot config f = Re.runReaderT (unBot f) config

handleMessage :: User -> String -> R.Redis ()
handleMessage u@(User suserId) text = do
    let userId = C.pack . show $ suserId

    let chunks = splitOn " " text :: [String]
    case (head chunks) of
        "/start"  -> saveUser userId >> return ()
        "/stop" -> removeUser userId >> return ()
        "/threshold" -> if length chunks == 2
            then (changeThreshold userId $ chunks !! 1) >> return ()
            else return ()
        "/help" -> (liftIO $ sendMessage u $ "/start\n/stop\n/threshold <num>")
            >> return ()
        _ -> return ()
  where
      saveUser userId = R.set userId "10"
      removeUser userId = R.del [userId]
      changeThreshold userId value = do
          (Right u) <- R.get userId
          case u of
              Just _ -> R.set userId (C.pack value) >> return ()
              Nothing -> return ()

server :: Bot ()
server = do
    conn <- fmap redisConn Re.ask

    liftIO $ scotty 8080 $ do
        post "/" $ do
            update <- jsonData :: ActionM Update
            liftIO $ do
                let (Update _ message) = update
                case message of
                    Just (Message _ _ Nothing) -> return ()
                    Just (Message _ user (Just text)) ->
                        R.runRedis conn (handleMessage user text)
                        >> return ()
                    Nothing -> return ()
            html "ok"

sendMessage (User userId) text = do
    -- FIXME (reading everytime is horrible)
    token <- fmap (head . splitOn "\n") $ readFile "token"
    let baseUrl = "https://api.telegram.org/bot"
    let url = mconcat [baseUrl, token, "/sendMessage",
                       "?chat_id=", (show userId), "&text=", text]

    req <- parseUrl url
    withManager tlsManagerSettings $ httpNoBody req

ancor :: Bot ()
ancor = do
    bot <- Re.ask

    _ <- liftIO . async . forever . liftIO $ do
        users <- R.runRedis (redisConn bot) $ do
            (Right ks) <- R.keys "*"
            mapM (\k -> do
                (Right t) <- R.get k
                return (k, fromJust t)) ks

        oldTop <- fmap botTop (readMVar $ botState bot)
        newTop <- HN.getTopStories

        mapM_ (f newTop oldTop bot) users

        threadDelay $ 60 * 1000 * 1000

    return ()
  where
    f newTop oldTop bot (uid, t) = do
        newsSent <- fmap botNewsSent (readMVar $ botState bot)

        let userId = read $ C.unpack uid
        let threshold = read $ C.unpack t

        let sent = if M.member userId newsSent
            then newsSent M.! userId
            else []

        let n = take threshold newTop
        let o = take threshold oldTop
        let diff = (n \\ o) \\ sent

        print diff
        let updatedNewsSent = M.insert userId (diff ++ sent) newsSent
        stories <- mapM HN.getStory diff

        mapM_ (send userId) stories
        modifyMVar_ (botState bot) $ \state -> do
            return $ state { botNewsSent = updatedNewsSent }

    send _ (HN.Story 0 _ _) =
        return ()

    send userId (HN.Story sid title url) = do
        let hnUrl = "https://news.ycombinator.com/item?id="
        sendMessage (User userId) $
            mconcat [title, " - ", url, " - ", hnUrl, (show sid)]
        return ()
