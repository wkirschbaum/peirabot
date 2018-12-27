module Peirabot.Modules.Quit (commandStop) where

import           Peirabot.Bot

commandStop :: BotInput -> BotAction
commandStop (BotInput input _ _)
  | "quit" == input = BotStop "goodbye"
  | otherwise = BotNoResult
