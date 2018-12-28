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
commandBye (BotInput input time rNum)
  | "bye" == input = BotResult 10 goodbye
  | "cheers" == input = BotResult 5 goodbye
  | "laters" == input = BotResult 5 goodbye
  | otherwise = BotNoResult
  where goodbye = getGoodbye (greetingForDay time) rNum

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

-- calculate levenshtein distance between two strings
levenshtein::[Char] -> [Char] -> Int
-- this part is mostly a speed optimiziation
levenshtein s1 s2
  | length s1 > length s2 = levenshtein s2 s1
  | length s1 < length s2 =
    let d = length s2 - length s1
    in d + levenshtein s1 (take (length s2 - d) s2)
-- the meat of the algorithm
levenshtein "" "" = 0
levenshtein s1 s2
  | last s1 == last s2 = levenshtein (init s1) (init s2)
  | otherwise = minimum [1 + levenshtein (init s1) s2,
                         1 + levenshtein s1 (init s2),
                         1 + levenshtein (init s1) (init s2)]
