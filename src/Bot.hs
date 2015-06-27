{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Bot where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
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

handleMessage :: User -> String -> R.Redis ()
handleMessage (User suserId) text = do
    let userId = C.pack . show $ suserId

    let chunks = splitOn " " text :: [String]
    case (head chunks) of
        "/start"  -> saveUser userId >> return ()
        "/stop" -> removeUser userId >> return ()
        "/threshold" -> (changeThreshold userId $ chunks !! 1) >> return ()
  where
      saveUser userId = R.set userId "10"
      removeUser userId = R.del [userId]
      changeThreshold userId value = do
          (Right u) <- R.get userId
          case u of
              Just _ -> R.set userId (C.pack value) >> return ()
              Nothing -> return ()

server conn = do
    scotty 8000 $ do
        post "/" $ do
            update <- jsonData :: ActionM Update
            liftIO $ do
                let (Update _ message) = update
                case message of
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

ancor :: R.Connection -> S.StateT (NewsSent, [Int]) IO ()
ancor conn = forever $ do
    users <- liftIO $ R.runRedis conn $ do
        (Right ks) <- R.keys "*"
        mapM (\k -> do
            (Right t) <- R.get k
            return (k, fromJust t)) ks

    (newsSent, oldTop) <- S.get
    newTop <- liftIO $ HN.getTopStories

    mapM_ (f newTop oldTop newsSent) users

    liftIO $ threadDelay $ 60 * 1000 * 1000
  where
    f newTop oldTop newsSent (uid, t) = do
        let userId = read $ C.unpack uid
        let threshold = read $ C.unpack t

        let sent = if M.member userId newsSent
            then newsSent M.! userId
            else []

        let diff = ((take threshold newTop) \\ (take threshold oldTop)) \\ sent

        let updatedNewsSent = M.insert userId (diff ++ sent) newsSent
        stories <- liftIO $ mapConcurrently HN.getStory diff

        mapM_ (liftIO . send userId) stories
        S.put (updatedNewsSent, newTop)

    send _ (HN.Story 0 _ _) =
        return ()

    send userId (HN.Story sid title url) = do
        let hnUrl = "https://news.ycombinator.com/item?id="
        sendMessage (User userId) $
            mconcat [title, " - ", url, " - ", hnUrl, (show sid)]
        return ()
