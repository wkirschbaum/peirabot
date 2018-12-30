module Peirabot.Modules.Greet
  (commandBye, commandHello)
where

import           Data.List
import           Data.Time.LocalTime
import           Peirabot.Bot

greeting :: BotContext -> IO String
greeting BotContext{time=time,randomNumber=random} = do
  return (getGreeting (greetingForDay time) random)

commandHello :: [BotMatch]
commandHello =
  [
    (BotStringMatch "hi", greeting),
    (BotStringMatch "hello", greeting),
    (BotStringMatch "morning", greeting),
    (BotStringStartWith "evening", greeting),
    (BotStringStartWith "afternoon", greeting)
  ]

commandBye :: [BotMatch]
commandBye =
  [
    (BotStringMatch "bye", goodbye),
    (BotStringMatch "cheers", goodbye),
    (BotStringStartWith "later", goodbye)
  ]

goodbye :: BotContext -> IO String
goodbye BotContext{time=time,randomNumber=random} = do
  return $ getGoodbye (greetingForDay time) random

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

getGoodbye :: DaySection -> Int -> String
getGoodbye Morning num =
  pickFromList num [
    "Have a good day",
    "Laters",
    "Cheers"
  ]
getGoodbye Afternoon num =
  pickFromList num [
    "Enjoy the rest of your day.",
    "Enjoy your afternoon",
    "Keep well"
  ]
getGoodbye Evening num =
  pickFromList num [
    "See you tomorrow",
    "Ciao."
  ]
