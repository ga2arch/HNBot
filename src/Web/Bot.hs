{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Web.Bot
        ( runBot
        , runAsync
        , getPort
        , getToken
        , getManager
        , getQueue
        , getCommands
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

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.ByteString.UTF8                  (fromString)
import           Data.Maybe                            hiding (listToMaybe)
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.TLS
import           System.Directory

import qualified Data.ByteString.Char8                 as C
import qualified Data.Map                              as M
import qualified Text.Parsec                           as P
import qualified Web.Scotty                            as Sc
import qualified Web.Scotty.TLS                        as ScTLS

import qualified Control.Concurrent.Lock               as L
import qualified Data.Text.Lazy                        as T
import qualified System.Posix                          as SP

import           Web.Bot.Types

runBot :: BotConfig -> Bot b -> IO b
runBot config f = do
    queue <- liftIO newChan
    conts <- liftIO $ newMVar M.empty

    let state = BotState [] queue conts
    evalStateT (runReaderT (unBot $ server >> f) config) state

getPort :: Bot Int
getPort = asks botPort

getToken :: Bot String
getToken = asks botToken

getManager :: Bot Manager
getManager = asks botManager

getQueue :: Bot (Chan Message)
getQueue = gets botQueue

getCommands :: Bot [Cmd]
getCommands = gets botCommands

getConts = gets botConts

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

    liftIO $ withMVar mAll $ \allConts -> do
        let (_, mUser) = allConts M.! user

        modifyMVar_ mUser $ \userConts ->
            return $ userConts ++ [parser]

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs

server :: Bot ()
server = do
    port  <- getPort
    queue <- getQueue
    conts <- getConts

    let startServer = ScTLS.scottyTLS port "cert/server.key" "cert/server.crt"
    liftIO . async . startServer $ do
        Sc.post "/" $ do
            update <- Sc.jsonData :: Sc.ActionM Update
            liftIO $ do
                print update

                let (Update _ message) = update
                whenJust message $
                    \m@Message { chat = chat,
                                 text = Just _ } -> do
                        addUser chat conts
                        writeChan queue m
            Sc.html "ok"

        Sc.get "/static/:name" $ do
            name <- Sc.param "name"
            path <- liftIO $ makeAbsolute $ "static/" ++ name

            Sc.setHeader "Content-Type" "application/force-download"
            Sc.file path

    return ()
  where
    addUser user conts = modifyMVar_ conts $ \m ->
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

        (lock, userConts) <- liftIO $ withMVar mAll $ \allConts -> do
            let (lock, mUser) = allConts M.! user
            L.acquire lock

            userConts <- liftIO $ readMVar mUser
            return (lock, userConts)

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
        liftIO $ withMVar mAll $ \allConts -> do
            let (_, mUser) = allConts M.! user

            modifyMVar_ mUser $ \(listToMaybe -> conts) ->
                maybe (return []) (\(_:xs) -> return xs) conts

call :: String -> [Part] -> Bot ()
call endpoint payload = do
    m    <- getManager
    url  <- prepReq endpoint
    req' <- parseUrl url

    req  <- formDataBody payload req'
    r <- liftIO $ try $ httpLbs req m
    case r of
        Left (ex :: SomeException) -> liftIO $ print ex
        Right _  -> return ()

  where
    prepReq endpoint = do
      token <- getToken

      let baseUrl = "https://api.telegram.org/bot"
          url = mconcat [baseUrl, token, "/", endpoint]

      return url

setWebhook :: String -> Bot ()
setWebhook host =
    call "setWebhook" [ partBS "url" (C.pack $ show host)]

send :: String -> Parser
send text = do
    u <- P.getState
    lift $ send' text u

send' :: String -> User -> Bot ()
send' text (User uid) =
    call "sendMessage" [ partBS "chat_id" (C.pack $ show uid)
                       , partBS "text"    (fromString text)
                       , partBS "disable_web_page_preview" "true"]

sendDoc :: FilePath -> Parser
sendDoc file = do
    u <- P.getState
    lift $ sendDoc' file u

sendDoc' :: FilePath -> User -> Bot ()
sendDoc' file (User uid) = do
    call "sendDocument" [ partBS "chat_id" (C.pack $ show uid)
                        , partFileSource "document" file ]
