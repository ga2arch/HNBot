{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, GeneralizedNewtypeDeriving,
             RankNTypes, ExistentialQuantification #-}
module Bot where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Maybe (fromJust)
import GHC.Generics
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as C
import qualified Web.Scotty as Sc
import qualified Control.Concurrent.Lock as L

data User = User {
    id :: Int
} deriving (Show, Generic, Ord, Eq)

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

newtype Bot a = B { unBot :: ReaderT BotConfig (StateT BotState IO) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig,
              MonadState BotState, MonadIO, MonadCatch, MonadThrow, MonadMask)

data BotConfig = BotConfig {
    botPort    :: Int
,   botToken   :: String
,   botManager :: Manager
,   botDbConn  :: R.Connection
}

<<<<<<< HEAD
data BotState = BotState {
    botNewsSent :: NewsSent
,   botCache    :: M.Map Int HN.Story
,   botTop      :: [Int]
} deriving (Show)
=======
data Cmd = Cmd {
    cmdParser :: P.ParsecT String User Bot ()
}
>>>>>>> rewrite

data BotState = forall a u. BotState {
    botCache       :: MVar a
,   botCommands    :: [Cmd]
,   botQueue       :: Chan Message
,   botConts       :: MVar (M.Map User (L.Lock, MVar [P.ParsecT String User Bot ()]))
}

runBot :: BotConfig -> Bot a -> IO a
runBot config f = do
    queue <- newChan
    cache <- newEmptyMVar
    conts <- newMVar M.empty

    let state = BotState cache [] queue conts
    evalStateT (runReaderT (unBot f) config) state


getPort :: Bot Int
getPort = asks botPort

getToken :: Bot String
getToken = asks botToken

getManager :: Bot Manager
getManager = asks botManager

getDbConn :: Bot R.Connection
getDbConn = asks botDbConn

getQueue :: Bot (Chan Message)
getQueue = gets botQueue

getCommands :: Bot [Cmd]
getCommands = gets botCommands

getConts = gets botConts

getUsers :: Bot [(User, C.ByteString)]
getUsers = do
    conn <- getDbConn
    users <- liftIO $ R.runRedis conn $ do
        ks <- query
        mapM userData ks

    return users
  where
    query = do
        Right ks <- R.keys "*"
        return ks

    userData k = do
        Right t <- R.get k
        return (User . toInt $ k,
                fromJust t)

    toInt :: C.ByteString -> Int
    toInt = read . C.unpack

withCache :: (forall a. a -> Bot (a, b)) -> Bot b
withCache f = do
    d <- get
    withCache' d f
  where
      withCache' :: BotState -> (forall a. a -> Bot (a, b)) -> Bot b
      withCache' (BotState m _ _ _) f =
          mask $ \restore -> do
              cache <- liftIO $ takeMVar m
              (newCache, result) <- restore (f cache)
                `onException` (liftIO $ putMVar m cache)
              liftIO $ putMVar m newCache
              return result

addCmd :: Cmd -> Bot ()
addCmd cmd = do
    s <- get
    let cmds = botCommands s
    put $ s { botCommands = cmd:cmds }

next parser = do
    u <- P.getState
    lift $ addCont u parser

addCont :: User -> P.ParsecT String User Bot () -> Bot ()
addCont user parser = do
    mAll <- getConts

    liftIO $ modifyMVar_ mAll $ \allConts -> do
        let (lock, mUser) = allConts M.! user

<<<<<<< HEAD
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
                let t = (read $ chunks !! 1) :: Int
                R.runRedis conn $ do
                    (Right u) <- R.get (C.pack $ show uid)
                    case u of
                        Just _ -> R.set (C.pack $ show uid) (C.pack $ show t) >> return ()
                        Nothing -> return ()
            else
                return ()

    topN m token state uid n = do
        ids <- fmap botTop $ readMVar state
        sendStory m token userId story) diff
        mapM_ (\s -> HN.getStory m s >>= sendStory m token uid) $ take n ids
=======
        modifyMVar_ mUser $ \userConts ->
            return $ userConts ++ [parser]

        return $ M.insert user (lock, mUser) allConts
>>>>>>> rewrite

server :: Bot ()
server = do
    port  <- getPort
    queue <- getQueue
    conts <- getConts

    liftIO . async . Sc.scotty port $ do
        Sc.post "/" $ do
            update <- Sc.jsonData :: Sc.ActionM Update
            liftIO $ do
                print update

                let (Update _ message) = update
                case message of
                    Just m@(Message _ user (Just text)) -> do
                        addUser user conts
                        writeChan queue m
                    _ -> return ()
            Sc.html "ok"
    return ()
  where
    addUser user conts = modifyMVar_ conts $ \m -> do
        if user `M.member` m
            then return m
            else do
                l <- L.new
                e <- newMVar []
                return $ M.insert user (l, e) m

handler :: String -> Bot ()
handler n = do
    config <- ask
    state  <- get

    liftIO . async $
        evalStateT (runReaderT (unBot $ handler' n) config) state
    return ()

handler' :: String -> Bot ()
handler' n = do
    queue <- getQueue

<<<<<<< HEAD
    req <- parseUrl url
    r <- try $ httpNoBody req m
    case r of
        Left (ex :: SomeException) -> print ex
        Right _  -> return ()
=======
    forever $ do
        Message _ user content <- liftIO $ readChan queue
        case content of
            Just text -> do
                liftIO $ print $ n ++ " : " ++ text
>>>>>>> rewrite

                mAll <- getConts

                (lock, userConts) <- liftIO $ modifyMVar mAll $ \allConts -> do
                    let (lock, mUser) = allConts M.! user
                    L.acquire lock

                    userConts <- liftIO $ readMVar mUser
                    return (allConts, (lock, userConts))

                runConts text user userConts
                    `finally` (liftIO $ L.release lock)

            Nothing   -> return ()

<<<<<<< HEAD
        mapM_ (process m token newTop oldTop bot) users
        modifyMVar_ (botState bot) $ \state -> do
            return $ state { botTop = newTop, botCache = M.empty }
=======
  where
    handleText :: String -> User -> Bot ()
    handleText text user = do
        cmds <- getCommands
        let parsers = map cmdParser cmds
        let parser  = P.choice $ map P.try parsers
>>>>>>> rewrite

        P.runParserT parser user "" text
        return ()

    runConts text user (c:xs) = do
        P.runParserT c user "" text
        delCont user

    runConts text user [] = do
        handleText text user

<<<<<<< HEAD
    process m token newTop oldTop bot (uid, t) = do
        newsSent <- fmap botNewsSent (readMVar $ botState bot)
        cache    <- fmap botCache    (readMVar $ botState bot)
=======
    delCont user = do
        mAll <- getConts
        allConts <- liftIO $ takeMVar mAll
>>>>>>> rewrite

        let (_, mUser) = allConts M.! user

        liftIO $ modifyMVar_ mUser $ \conts -> do
            case conts of
                []     -> return []
                (_:xs) -> return xs

        liftIO $ putMVar mAll allConts

send text = do
    u <- P.getState
    lift $ send' text u

<<<<<<< HEAD
            mapM_ (\i -> do
                story <- if M.member i cache
                    then return $ cache M.! i
                    else do
                        story <- HN.getStory m i
                        let updatedCache = M.insert i story cache

                        modifyMVar_ (botState bot) $ \state -> do
                            return $ state { botCache = updatedCache }

                        return story

                sendStory m token userId story) diff

=======
send' :: String -> User -> Bot ()
send' text u@(User uid) = do
    m     <- getManager
    token <- getToken
>>>>>>> rewrite

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
