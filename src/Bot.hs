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

data BotState = forall a u. BotState {
    botCache       :: MVar a
,   botCommands    :: [Cmd]
,   botQueue       :: Chan Message
,   botConts       :: M.Map User [P.ParsecT String User Bot ()]
}

runBot :: BotConfig -> Bot a -> IO a
runBot config f = do
    queue <- newChan
    cache <- newEmptyMVar

    let state = BotState cache [] queue M.empty
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

getConts :: User -> Bot [P.ParsecT String User Bot ()]
getConts user = do
    conts <- gets botConts
    if user `M.member` conts
        then return $ conts M.! user
        else return []

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

addCont :: User -> P.ParsecT String User Bot () -> Bot ()
addCont user parser = do
    conts <- getConts user

    modify $ \s -> do
        let newConts = conts ++ [parser]
        s { botConts = M.insert user newConts $ botConts s }

delCont :: User -> Bot ()
delCont user = do
    (_:newConts) <- getConts user
    modify $ \s -> do
        s { botConts =  M.insert user newConts $ botConts s }

server :: Bot ()
server = do
    port  <- getPort
    queue <- getQueue

    liftIO . async . Sc.scotty port $ do
        Sc.post "/" $ do
            update <- Sc.jsonData :: Sc.ActionM Update
            liftIO $ do
                print update

                let (Update _ message) = update
                case message of
                    Just m@(Message _ user (Just text)) -> writeChan queue m
                    _ -> return ()
            Sc.html "ok"
    return ()

handler :: Bot ()
handler = do
    queue <- getQueue

    forever $ do
        Message _ user content <- liftIO $ readChan queue
        case content of
            Just text -> do
                conts <- getConts user
                runConts text user conts
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
