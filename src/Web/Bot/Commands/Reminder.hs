{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Bot.Commands.Reminder
        ( reminder
        )where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Text.Parsec               as P
import           Web.Bot

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
            u    <- P.getState

            lift . runAsync $ do
                liftIO $ threadDelay (microseconds time unit)
                send' text u

            send "✅"
  where
      microseconds time unit = time *
          case unit of
              's' -> 1  * 10^6
              'm' -> 6  * 10^7
              'h' -> 36 * 10^8
