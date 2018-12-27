module Modules.Quit (commandStop) where

import           Bot

commandStop :: String -> BotAction
commandStop input
  | "quit" == input = BotStop "goodbye"
  | otherwise = BotNoResult
