module Peirabot.Bot (
  Score,
  Output,
  BotAction(..),
  BotInput(..)
  ) where

import           Data.Time.LocalTime

type Score = Int
type Output = String
data BotAction = BotNoResult | BotResult Score Output | BotStop Output deriving (Show, Eq, Ord)

type InputString = String
type RandomNumber = Int
data BotInput = BotInput String ZonedTime RandomNumber
