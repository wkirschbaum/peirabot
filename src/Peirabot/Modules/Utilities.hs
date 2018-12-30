module Peirabot.Modules.Utilities
    ( commandTime, commandRandom
    ) where

import           Data.Time.Format
import           Peirabot.Bot

commandTime :: BotInput -> BotAction
commandTime (BotInput input)
  | "time" == input = BotResult 10 displayTime
  | otherwise = BotNoResult

commandRandom :: BotInput -> BotAction
commandRandom (BotInput input)
  | "random" == input = BotResult 10 displayRandom
  | otherwise = BotNoResult

displayTime :: BotContext -> IO String
displayTime BotContext{time=time} = do
  return (formatTime defaultTimeLocale "%d %B %Y %R" time)

displayRandom :: BotContext -> IO String
displayRandom BotContext{randomNumber=random} = do
  return (show random)
