module Peirabot.Modules.Greet
  (commandHello, commandBye) where

import           Data.List
import           Data.Time.LocalTime
import           Peirabot.Bot

commandHello :: BotInput -> BotAction
commandHello (BotInput input time rNum)
  | "hi" `isPrefixOf` input = BotResult 10 greeting
  | "hello" `isPrefixOf` input = BotResult 10 greeting
  | "morning" == input  =  BotResult 10 greeting
  | "evening" == input  =  BotResult 10 greeting
  | "afternoon" == input  =  BotResult 10 greeting
  | otherwise = BotNoResult
  where greeting = getGreeting (greetingForDay time) rNum

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

pickFromList :: Int -> [a] -> a
pickFromList r lst =
  lst !! i
  where i = r `mod` (length lst)

getGreeting :: DaySection -> Int -> String
getGreeting Morning num =
  pickFromList num [
    "Good morning!",
    "Morning buddy.",
    "Hi.",
    "Good to see you."
  ]
getGreeting Afternoon num =
  pickFromList num [
    "Good afternoon!",
    "Howzit.",
    "Hey.",
    "What's up?"
  ]
getGreeting Evening num =
  pickFromList num [
    "Good evening!",
    "Hi.",
    "Hey there."
  ]
