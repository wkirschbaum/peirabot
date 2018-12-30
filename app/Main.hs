module Main where

import           Data.Time.LocalTime
import           Peirabot.Bot
import           Peirabot.Modules.Greet
import           Peirabot.Modules.Quit
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
  case command (BotInput l) of
    BotResult _ action -> do
      context <- (getContext l)
      result <- action context
      putStrLn result
      repLoop
    BotStop r -> do
      return ()
    _ -> repLoop

command :: BotInput -> BotAction
command input = maximum $ ($ input) <$> [
  commandStop,
  commandHello,
  commandBye,
  commandTime,
  commandRandom
  ]

getContext :: String -> IO BotContext
getContext i = do
  t <- getZonedTime
  r <- getStdRandom (randomR (1,1000000))
  return BotContext { time = t, randomNumber = r, input = i}
