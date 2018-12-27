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
  l <- getLine
  t <- getZonedTime
  r <- getStdRandom (randomR (1,1000000)) -- Somehow allow this to have dynamic size
  case command (BotInput l t r) of
    BotStop r -> do
      putStrLn r
      return ()
    BotResult _ r -> do
      putStrLn r
      repLoop
    _ -> repLoop

command :: BotInput -> BotAction
command input = maximum $ ($ input) <$> [
  commandStop,
  commandHello,
  commandTime,
  commandRandom,
  commandBye
  ]
