{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables,
             FlexibleContexts, GeneralizedNewtypeDeriving,
             RankNTypes, ExistentialQuantification #-}
module Web.Bot
        ( runBot
        , runAsync
        , getPort
        , getToken
        , getManager
        , getDbConn
        , getQueue
        , getCommands
        , getUsers
        , addCmd
        , addCont

        , server
        , send
        , send'
        , sendDoc
        , next
        , handler
        , User (..)
        , Message (..)
        , BotConfig (..)
        , Parser
        , Bot
        )where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Builder (byteString)
import Data.Maybe (fromJust)
import GHC.Generics
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import System.Directory

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as C
import qualified Web.Scotty as Sc
import qualified Control.Concurrent.Lock as L
import qualified System.Posix as SP
import qualified Data.Text.Lazy as T

import Web.Bot.Types

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

addCmd :: Parser -> Bot ()
addCmd cmd = do
    s <- get
    let cmds = botCommands s
    put $ s { botCommands = Cmd cmd:cmds }

next :: Parser -> Parser
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
                    Just m@Message { chat = chat,
                                     text = Just _ } -> do
                        addUser chat conts
                        writeChan queue m
                    _ -> return ()
            Sc.html "ok"

        Sc.get "/static/:name" $ do
            name <- Sc.param "name"
            path <- liftIO $ makeAbsolute $ "static/" ++ name

            Sc.setHeader "Content-Type" "application/force-download"
            Sc.file path

    return ()
  where
    addUser user conts = modifyMVar_ conts $ \m -> do
        if user `M.member` m
            then return m
            else do
                l <- L.new
                e <- newMVar []
                return $ M.insert user (l, e) m

runAsync :: Bot a -> Bot ()
runAsync f = do
    config <- ask
    state  <- get

    liftIO . async $
        evalStateT (runReaderT (unBot f) config) state
    return ()

handler :: String -> Bot ()
handler n = do
    queue <- getQueue

    forever $ do
        Message { chat = user,
                  text = Just text } <- liftIO $ readChan queue

        liftIO $ print $ n ++ " : " ++ text

        mAll <- getConts

        (lock, userConts) <- liftIO $ modifyMVar mAll $ \allConts -> do
            let (lock, mUser) = allConts M.! user
            L.acquire lock

            userConts <- liftIO $ readMVar mUser
            return (allConts, (lock, userConts))

        runConts text user userConts
            `finally` (liftIO $ L.release lock)

  where
    handleText :: String -> User -> Bot ()
    handleText text user = do
        cmds <- getCommands
        let parser  = P.choice $ map (P.try . cmdParser) cmds

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

send :: String -> Parser
send text = do
    u <- P.getState
    lift $ send' text u

send' :: String -> User -> Bot ()
send' text u@(User uid) = do
    m     <- getManager
    token <- getToken

    let baseUrl = "https://api.telegram.org/bot"
        payload = urlEncodeVars [("chat_id", (show uid))
                                ,("text", text)
                                ,("disable_web_page_preview", "true")]
        url = mconcat [baseUrl, token, "/sendMessage?", payload]

    req <- parseUrl url
    r <- liftIO $ try $ httpLbs req m
    case r of
        Left (ex :: SomeException) -> liftIO $ print ex
        Right _  -> return ()

sendDoc :: FilePath -> Parser
sendDoc file = do
    u <- P.getState
    lift $ sendDoc' file u

sendDoc' :: FilePath -> User -> Bot ()
sendDoc' file (User uid) = do
    m     <- getManager
    token <- getToken

    let baseUrl = "https://api.telegram.org/bot"
        url = mconcat [baseUrl, token, "/sendDocument"]

    req' <- parseUrl url
    req  <- formDataBody [partBS "chat_id" (C.pack $ show uid)
                          ,partFileSource "document" file] req'
    r <- liftIO $ try $ httpLbs req m
    case r of
        Left (ex :: SomeException) -> liftIO $ print ex
        Right _  -> return ()
