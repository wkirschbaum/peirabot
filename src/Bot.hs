module Bot (
  Score,
  Output,
  BotAction(..)
  ) where

type Score = Int
type Output = String
data BotAction = BotNoResult | BotStop Output | BotResult Score Output deriving (Show)

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
