{-# LANGUAGE OverloadedStrings #-}
module Web.Bot.Commands.Help
        ( help
        ) where

import Data.List
import Web.Bot
import qualified Text.Parsec as P

help :: Parser
help = do
    P.string "/help"
    let msg = ["/top3", "/top5", "/top10", "/threshold - Sets the news threshold"]
    send $ mconcat $ intersperse "\n" msg
