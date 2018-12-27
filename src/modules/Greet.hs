module Modules.Greet
  (commandHello, commandBye) where

import           Bot
import           Data.List

commandHello :: String -> BotAction
commandHello input
  | "hi" `isPrefixOf` input = BotResult 2 "Hi :)"
  | "hello" `isPrefixOf` input = BotResult 2 "Hello :)"
  | "morning" == input  = BotResult 10 "Hello :)"
  | "evening" == input  = BotResult 10 "Hello :)"
  | "afternoon" == input  = BotResult 10 "Hello :)"
  | otherwise = BotNoResult

commandBye :: String -> BotAction
commandBye input
  | "bye" == input = BotResult 10 "Bye, see you later."
  | "cheers" == input = BotResult 5 "Cheers, laters."
  | otherwise = BotNoResult
