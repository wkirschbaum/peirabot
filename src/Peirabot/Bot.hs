module Peirabot.Bot (
  Score,
  Output,
  BotAction(..),
  BotInput(..),
  BotContext(..)
  ) where

import           Data.Text.Metrics
import           Data.Time.LocalTime

type Score = Int
type Output = BotContext -> IO String

data BotAction = BotNoResult
               | BotResult Score Output
               | BotStop String
 
instance Show BotAction where
  show (BotResult score _) = show score
  show (BotNoResult) = "No result"
  show (BotStop reason) = reason

instance Eq BotAction where
  (BotResult score1 _) == (BotResult score2 _) = score1 == score2
  BotNoResult == BotNoResult = True
  (BotStop reason1) == (BotStop reason2) = reason1 == reason2
  _ == _ = False

instance Ord BotAction where
  (BotResult score1 _) `compare` (BotResult score2 _) = score1 `compare` score2
  BotNoResult `compare` _ = LT
  _ `compare` BotNoResult = GT
  (BotStop _) `compare` _ = GT
  _ `compare` (BotStop _) = LT
  
data BotContext = BotContext {time :: ZonedTime, randomNumber :: Int}
data BotInput = BotInput String
