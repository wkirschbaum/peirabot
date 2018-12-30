module Main where

import           Data.List
import           Data.Time.LocalTime
import           Peirabot.Bot
import           Peirabot.Modules.Greet
import           Peirabot.Modules.Utilities
import           System.IO
import           System.Random

main :: IO ()
main = do
  intro
  repLoop

intro :: IO ()
intro = do
  putStrLn "I am ready."

repLoop :: IO ()
repLoop = do
  putStr "> "
  -- ensure buffer is clear before receiving input
  hFlush stdout
  l <- getLine
  case command l of
    BotResult _ action -> do
      context <- (getContext l)
      result <- action context
      putStrLn result
      repLoop
    BotStop r -> do
      return ()
    _ -> repLoop

command :: String -> BotAction
command input = maximum $ (doCommand input) <$> [
  commandHello,
  commandBye,
  commandUtilities
  ]

doCommand :: String -> [BotMatch] -> BotAction
doCommand "quit" _ = BotStop "Bye, I am done."
doCommand _ [] = BotNoResult
doCommand input (((BotStringMatch match), output):xs)
  | input == match = (BotResult 10 output)
  | otherwise = doCommand input xs
doCommand input (((BotStringStartWith match), output):xs)
  | (match `isPrefixOf` input) = BotResult 9 output
  | otherwise = doCommand input xs
  
getContext :: String -> IO BotContext
getContext i = do
  t <- getZonedTime
  r <- getStdRandom (randomR (1,1000000))
  return BotContext { time = t, randomNumber = r, input = i}
