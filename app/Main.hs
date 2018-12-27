module Main where

import           Bot
import           Data.List
import           System.IO

main :: IO ()
main = do
  intro
  repLoop

intro :: IO ()
intro = do
  putStrLn "I am ready."

-- TODO:  add context in the repl

repLoop :: IO ()
repLoop = do
  putStr "> "
  hFlush stdout
  l <- getLine
  case command l of
    BotStop r -> do
      putStrLn r
      return ()
    BotResult _ r -> do
      putStrLn r
      repLoop
    _ -> repLoop

command :: String -> BotAction
command input = maximum $ ($ input) <$> [
  commandStop,
  commandHello,
  commandBye
  ]

commandStop :: String -> BotAction
commandStop input
  | "quit" == input = BotStop "goodbye"
  | otherwise = BotNoResult

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
