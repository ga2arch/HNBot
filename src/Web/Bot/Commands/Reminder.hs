{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts,
             ScopedTypeVariables #-}

module Web.Bot.Commands.Reminder
        ( reminder
        )where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Web.Bot
import qualified Text.Parsec as P

reminder = do
    P.string "/remind"

    send "Remind in: (ex. 10s - 10m - 1h)"
    next $ do
        let units = map (P.try . P.char) "smh"
        time <- read <$> P.many1 P.digit
        unit <- P.choice units

        send "What to remind: "

        next $ do
            text <- P.many1 P.anyToken
            u <- P.getState

            lift . runAsync $ do
                liftIO $ threadDelay (converTime time unit)
                send' text u

            send "✅"
  where
    converTime time 's' = time * 10^6
    converTime time 'm' = converTime (time * 10^6) 's'
    converTime time 'h' = converTime (time * 10^6) 'm'
