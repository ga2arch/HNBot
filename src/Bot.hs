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

data Cmd = Cmd {
    cmdParser :: P.ParsecT String User Bot ()
}

data BotState = forall a. BotState {
    botCommands    :: [Cmd]
,   botQueue       :: Chan Message
,   botConts       :: MVar (M.Map User (L.Lock, MVar [P.ParsecT String User Bot ()]))
}

runBot :: BotConfig -> Bot b -> IO b
runBot config f = do
    queue <- newChan
    conts <- newMVar M.empty

    let state = BotState [] queue conts
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

-- withCache :: (forall a. a -> Bot (a, b)) -> Bot b
-- withCache f = do
--     d <- get
--     withCache' d f
--   where
--       withCache' :: BotState -> (forall a. a -> Bot (a, b)) -> Bot b
--       withCache' (BotState m _ _ _) f = do
--           mask $ \restore -> do
--               cache <- liftIO $ takeMVar m :: Bot a
--               (newCache :: a, result) <- restore (f cache)
--                 `onException` (liftIO $ putMVar m cache)
--               liftIO $ putMVar m newCache
--               return result

--addCmd :: Cmd -> Bot ()
addCmd cmd = do
    s <- get
    let cmds = botCommands s
    put $ s { botCommands = Cmd cmd:cmds }

next parser = do
    u <- P.getState
    lift $ addCont u parser

addCont :: User -> P.ParsecT String User Bot () -> Bot ()
addCont user parser = do
    mAll <- getConts

    liftIO $ modifyMVar_ mAll $ \allConts -> do
        let (lock, mUser) = allConts M.! user

        modifyMVar_ mUser $ \userConts ->
            return $ userConts ++ [parser]

        return $ M.insert user (lock, mUser) allConts

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
handler n = runAsync $ handler' n

runAsync f = do
    config <- ask
    state  <- get

    liftIO . async $
        evalStateT (runReaderT (unBot f) config) state
    return ()

handler' :: String -> Bot ()
handler' n = do
    queue <- getQueue

    forever $ do
        Message _ user content <- liftIO $ readChan queue
        case content of
            Just text -> do
                liftIO $ print $ n ++ " : " ++ text

                mAll <- getConts

                (lock, userConts) <- liftIO $ modifyMVar mAll $ \allConts -> do
                    let (lock, mUser) = allConts M.! user
                    L.acquire lock

                    userConts <- liftIO $ readMVar mUser
                    return (allConts, (lock, userConts))

                runConts text user userConts
                    `finally` (liftIO $ L.release lock)

            Nothing   -> return ()

  where
    handleText :: String -> User -> Bot ()
    handleText text user = do
        cmds <- getCommands
        let parsers = map cmdParser cmds
        let parser  = P.choice $ map P.try parsers

        P.runParserT parser user "" text
        return ()

    runConts text user (c:xs) = do
        P.runParserT c user "" text
        delCont user

    runConts text user [] = do
        handleText text user

    delCont user = do
        mAll <- getConts
        allConts <- liftIO $ takeMVar mAll

        let (_, mUser) = allConts M.! user

        liftIO $ modifyMVar_ mUser $ \conts -> do
            case conts of
                []     -> return []
                (_:xs) -> return xs

        liftIO $ putMVar mAll allConts

send text = do
    u <- P.getState
    lift $ send' text u

send' :: String -> User -> Bot ()
send' text u@(User uid) = do
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
