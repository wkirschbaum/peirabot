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
  hFlush stdout
  c <- context
  l <- getLine
  case command (BotInput l) of
    BotResult _ action -> do
      result <- action c
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

topAction :: BotAction -> BotAction -> BotAction
topAction BotNoResult right = right
topAction left BotNoResult = left
topAction (BotStop x) _ = BotStop x
topAction _ (BotStop x) = BotStop x
  

context :: IO BotContext
context = do
  t <- getZonedTime
  r <- getStdRandom (randomR (1,1000000))
  return BotContext { time = t, randomNumber = r}
