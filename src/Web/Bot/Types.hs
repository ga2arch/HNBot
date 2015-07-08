{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Web.Bot.Types where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client

import qualified Control.Concurrent.Lock as L
import qualified Data.Map    as M
import qualified Text.Parsec as P

data User = User {
    id :: Int
} deriving (Show, Generic, Ord, Eq)

instance FromJSON User
instance ToJSON   User

data Message = Message {
    message_id :: Int
,   from :: User
,   chat :: User
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

-- class (Monad m) => MonadBot m where
--     send  :: String -> P.ParsecT s User m ()

newtype Bot a = B { unBot :: ReaderT BotConfig (StateT BotState IO) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig,
              MonadState BotState, MonadIO, MonadCatch, MonadThrow, MonadMask)

data BotConfig = BotConfig {
    botPort    :: Int
,   botToken   :: String
,   botManager :: Manager
}

type Parser = P.ParsecT String User Bot ()

data Cmd = Cmd {
    cmdParser :: Parser
}

data BotState = BotState {
    botCommands    :: [Cmd]
,   botQueue       :: Chan Message
,   botConts       :: MVar (M.Map User (L.Lock, MVar [Parser]))
}
