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

data BotData = BotData {
    redisConn :: R.Connection
,   botPort   :: Int
,   botToken  :: String
,   botState  :: MVar BotState
,   botChan   :: MVar Message
}

data BotState = BotState {
    botNewsSent :: NewsSent
,   botTop      :: [Int]
} deriving (Show)

newtype Bot a = Bot { unBot :: Re.ReaderT BotData IO a }
    deriving (Monad, Applicative, Functor,
              Re.MonadIO, Re.MonadReader BotData)

runBot config f = Re.runReaderT (unBot f) config

handler :: Bot ()
handler = do
    chan <- fmap botChan Re.ask
    conn <- fmap redisConn Re.ask

    liftIO . async . forever $ do
        Message _ (User uid) content <- takeMVar chan

        case content of
            Just text -> handleText conn uid text
            Nothing   -> return ()

    return ()
  where
    handleText conn uid ('/':cmd) = do
        let chunks = splitOn " " cmd
        case (head chunks) of
            "start"     -> addUser conn uid
            "stop"      -> delUser conn uid
            "help"      -> helpCmd uid
            "threshold" -> changeThreshold conn uid chunks
            _           -> return ()

    handleText _ _ _ = return ()

    addUser conn uid = do
        R.runRedis conn $ R.set (C.pack $ show uid) "10"
        return ()

    delUser conn uid = do
        R.runRedis conn $ R.del [C.pack $ show uid]
        return ()

    helpCmd uid = do
        let hm = mconcat [ "/start - Start receving news", "\n",
                           "/stop  - Stop receiving news", "\n",
                           "/threshold num - Set news threshold"]
        sendMessage uid hm

    changeThreshold conn uid chunks = do
        if length chunks > 1
            then do
                let t = chunks !! 1
                R.runRedis conn $ do
                    (Right u) <- R.get (C.pack $ show uid)
                    case u of
                        Just _ -> R.set (C.pack $ show uid) (C.pack t) >> return ()
                        Nothing -> return ()
            else
                return ()

server :: Bot ()
server = do
    conn <- fmap redisConn Re.ask
    port <- fmap botPort   Re.ask
    chan <- fmap botChan   Re.ask

    liftIO . scotty port $ do
        post "/" $ do
            update <- jsonData :: ActionM Update
            liftIO $ do
                let (Update _ message) = update
                case message of
                    Just m@(Message _ user (Just text)) -> putMVar chan m
                    _ -> return ()
            html "ok"

sendMessage userId text = do
    -- FIXME (reading everytime is horrible)
    token <- fmap (head . splitOn "\n") $ readFile "token"
    let baseUrl = "https://api.telegram.org/bot"
    let url = mconcat [baseUrl, token, "/sendMessage",
                       "?chat_id=", (show userId), "&text=", text]

    req <- parseUrl url
    withManager tlsManagerSettings $ httpNoBody req
    return ()

ancor :: Bot ()
ancor = do
    bot <- Re.ask

    liftIO . async . forever $ do
        users <- getUsers $ redisConn bot

        oldTop <- fmap botTop (readMVar $ botState bot)
        newTop <- HN.getTopStories

        mapM_ (process newTop oldTop bot) users

        threadDelay $ 60 * 1000 * 1000

    return ()
  where
    getUsers conn = R.runRedis conn $ do
        (Right ks) <- R.keys "*"
        mapM (\k -> do
            (Right t) <- R.get k
            return (k, fromJust t)) ks

    process newTop oldTop bot (uid, t) = do
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
