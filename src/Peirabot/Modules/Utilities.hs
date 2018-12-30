module Peirabot.Modules.Utilities
    ( commandUtilities
    ) where

import           Data.Time.Format
import           Peirabot.Bot

commandUtilities :: [BotMatch]
commandUtilities =
  [
    (BotStringMatch "time", displayTime),
    (BotStringMatch "random", displayRandom)
  ]

displayTime :: BotContext -> IO String
displayTime BotContext{time=time} = do
  return (formatTime defaultTimeLocale "%d %B %Y %R" time)

displayRandom :: BotContext -> IO String
displayRandom BotContext{randomNumber=random} = do
  return (show random)
