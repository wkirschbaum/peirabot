module Main where

import           Bot
import           Modules.Greet
import           Modules.Quit
import           Modules.Time
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
  commandTime,
  commandBye
  ]
