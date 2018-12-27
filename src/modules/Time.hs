module Modules.Time
    ( commandTime
    ) where

import           Bot

commandTime :: String -> BotAction
commandTime input
  | "time" == input = BotResult 10 "It's time"
  | otherwise = BotNoResult
