module Peirabot.Modules.Utilities
    ( commandTime, commandRandom
    ) where

import           Data.Time.Format
import           Peirabot.Bot

commandTime :: BotInput -> BotAction
commandTime (BotInput input time _)
  | "time" == input = BotResult 10 (formatTime defaultTimeLocale "%d %B %Y %R" time)
  | otherwise = BotNoResult

commandRandom :: BotInput -> BotAction
commandRandom (BotInput input _ random)
  | "random" == input = BotResult 10 (show random)
  | otherwise = BotNoResult
