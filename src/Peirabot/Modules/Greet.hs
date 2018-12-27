module Peirabot.Modules.Greet
  (commandHello, commandBye) where

import           Data.List
import           Data.Time.LocalTime
import           Peirabot.Bot

commandHello :: BotInput -> BotAction
commandHello (BotInput input _ _)
  | "hi" `isPrefixOf` input = BotResult 2 "Hi :)"
  | "hello" `isPrefixOf` input = BotResult 2 "Hello :)"
  | "morning" == input  = BotResult 10 "Hello :)"
  | "evening" == input  = BotResult 10 "Hello :)"
  | "afternoon" == input  = BotResult 10 "Hello :)"
  | otherwise = BotNoResult

commandBye :: BotInput -> BotAction
commandBye (BotInput input _ _)
  | "bye" == input = BotResult 10 "Bye, see you later."
  | "cheers" == input = BotResult 5 "Cheers, laters."
  | otherwise = BotNoResult

data DaySection = Morning | Afternoon | Evening deriving (Eq, Ord)

greetingForDay :: ZonedTime -> DaySection
greetingForDay (ZonedTime (LocalTime _ (TimeOfDay hour _ _)) zone)
  | hour > 18 = Evening
  | hour > 12 = Afternoon
  | otherwise = Morning
