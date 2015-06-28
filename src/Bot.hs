{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Bot where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Maybe (fromJust)
import Data.List
import Data.List.Split (splitOn)
import GHC.Generics
import Web.Scotty
import Network.HTTP.Base (urlEncodeVars)
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
    redisConn  :: R.Connection
,   botPort    :: Int
,   botToken   :: String
,   botState   :: MVar BotState
,   botChan    :: Chan Message
,   botManager :: Manager
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
    chan  <- Re.asks botChan
    conn  <- Re.asks redisConn
    state <- Re.asks botState
    m     <- Re.asks botManager
    token <- Re.asks botToken

    liftIO . async . forever $ do
        Message _ (User uid) content <- readChan chan

        case content of
            Just text -> do
                tryAny $ handleText m token conn state uid text
                return ()
            Nothing   -> return ()

    return ()
  where
    handleText m token conn state uid cmd = do
        let chunks = splitOn " " cmd
        case (head chunks) of
            "/start"     -> addUser conn uid
            "/stop"      -> delUser conn uid
            "/help"      -> helpCmd m token uid
            "/threshold" -> changeThreshold conn uid chunks
            "/top3"      -> topN m token state uid 3
            "/top5"      -> topN m token state uid 5
            "/top10"     -> topN m token state uid 10
            _           -> return ()

    addUser conn uid = do
        R.runRedis conn $ R.set (C.pack $ show uid) "10"
        return ()

    delUser conn uid = do
        R.runRedis conn $ R.del [C.pack $ show uid]
        return ()

    helpCmd m token uid = do
        let hm = mconcat [ "/start - Start receving news", "\n",
                           "/stop  - Stop receiving news", "\n",
                           "/threshold num - Set threshold for the news to receive", "\n",
                           "/top3 - Sends you the top3", "\n",
                           "/top5 - Sends you the top5", "\n",
                           "/top10 - Sends you the top10"]
        sendMessage m token uid hm

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

    topN m token state uid n = do
        ids <- fmap botTop $ readMVar state
        mapM_ (\s -> HN.getStory m s >>= sendStory m token uid) $ take n ids

server :: Bot ()
server = do
    conn <- Re.asks redisConn
    port <- Re.asks botPort
    chan <- Re.asks botChan

    liftIO . scotty port $ do
        post "/" $ do
            update <- jsonData :: ActionM Update
            liftIO $ do
                print update

                let (Update _ message) = update
                case message of
                    Just m@(Message _ user (Just text)) -> writeChan chan m
                    _ -> return ()
            html "ok"

sendMessage :: Manager -> String -> Int -> String -> IO ()
sendMessage m token userId text = do
    let baseUrl = "https://api.telegram.org/bot"
    let payload = urlEncodeVars [("chat_id", (show userId)),
                                 ("text", text),
                                 ("disable_web_page_preview", "true")]
    let url = mconcat [baseUrl, token, "/sendMessage?", payload]

    req <- parseUrl url
    r <- try $ httpLbs req m
    case r of
        Left (ex :: SomeException) -> print ex
        Right _  -> return ()

sendStory _ _ _ (HN.Story 0 _ _) = return ()
sendStory m token userId (HN.Story sid title url) = do
    let hnUrl = "https://news.ycombinator.com/item?id="
    sendMessage m token userId $
        mconcat [title, "\n\n",
                 url,   "\n\n",
                 hnUrl, (show sid)]
    return ()

tryAny :: NFData a => IO a -> IO (Either SomeException a)
tryAny action = f $ do
    res <- action
    evaluate $!! res
  where
      f action = withAsync action waitCatch

ancor :: Bot ()
ancor = do
    bot   <- Re.ask
    m     <- Re.asks botManager
    token <- Re.asks botToken


    liftIO . async . forever $ do
        users <- getUsers $ redisConn bot

        oldTop <- fmap botTop (readMVar $ botState bot)
        newTop <- HN.getTopStories m

        mapM_ (process m token newTop oldTop bot) users

        modifyMVar_ (botState bot) $ \state -> do
            return $ state { botTop = newTop }

        threadDelay $ 60 * 1000 * 1000

    return ()
  where
    getUsers conn = R.runRedis conn $ do
        Right ks <- R.keys "*"
        mapM (\k -> do
            Right t <- R.get k
            return (k, fromJust t)) ks

    process m token newTop oldTop bot (uid, t) = do
        newsSent <- fmap botNewsSent (readMVar $ botState bot)

        let userId = read $ C.unpack uid
        let threshold = read $ C.unpack t

        let sent = if M.member userId newsSent
            then newsSent M.! userId
            else []

        let n = take threshold newTop
        let o = take threshold oldTop
        let diff = (n \\ o) \\ sent
        let updatedNewsSent = M.insert userId (diff ++ sent) newsSent

        tryAny $ mapM_ (\s -> HN.getStory m s >>= sendStory m token userId) diff

        modifyMVar_ (botState bot) $ \state -> do
            return $ state { botNewsSent = updatedNewsSent }
