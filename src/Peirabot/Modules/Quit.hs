module Peirabot.Modules.Quit (commandStop) where

import           Peirabot.Bot

commandStop :: BotInput -> BotAction
commandStop (BotInput input)
  | "quit" == input = BotStop "goodbye"
  | otherwise = BotNoResult
