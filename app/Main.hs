module Main where

import Lib
import Data.List
import Control.Monad
import Data.Char
import Data.Foldable (asum)

type Score = Int
type Output = String
data BotAction = BotNoResult | BotStop Output | BotResult Score Output deriving (Show)

main :: IO ()
main = do
  putStr "> "
  l <- getLine
  case command l of
    BotStop r -> do
      putStrLn r
      return ()
    BotResult _ r -> do
      putStrLn r
      main
    _ -> main

command :: String -> BotAction
command input = maximum $ ($ input) <$> [
  commandHello,
  commandStop
  ]

commandHello :: String -> BotAction
commandHello input
  | "hi" `isPrefixOf` input = BotResult 1 "Hello :)"
  | otherwise = BotNoResult

commandStop :: String -> BotAction
commandStop input
  | "quit" == input = BotStop "goodbye"
  | otherwise = BotNoResult


instance Eq BotAction where
  BotStop _ == BotStop _ = True
  BotNoResult == BotNoResult = True
  (BotResult s1 _) == (BotResult s2 _) = s1 == s2

instance Ord BotAction where
  compare (BotStop _) _ = GT
  compare BotNoResult _ = LT
  compare (BotResult _ _) BotNoResult = GT
  compare (BotResult s1 _) (BotResult s2 _)
    | s1 > s2 = GT
    | s1 == s2 = EQ
    | otherwise = LT
