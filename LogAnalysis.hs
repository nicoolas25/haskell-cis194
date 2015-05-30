{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import qualified Data.String as Str

-- Execise 1

parseMessage :: String -> LogMessage
parseMessage string = case Str.words string of
  "E":s:ts:m -> LogMessage (Error (read s)) (read ts) (Str.unwords m)
  "I":ts:m -> LogMessage Info (read ts) (Str.unwords m)
  "W":ts:m -> LogMessage Warning (read ts) (Str.unwords m)
  _ -> Unknown string

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m (Node left n right) =
  if ts_m > ts_n then Node left n (insert m right) else Node (insert m left) n right
  where ts_m = timestamp m
        ts_n = timestamp n
        timestamp :: LogMessage -> TimeStamp
        timestamp (LogMessage _ ts _) = ts
        timestamp _ = error "MessageTree only should only contains LogMessage"

-- Exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = onlyMessage . filterLog . inOrder . build
  where filterLog = filter isSevereError
        onlyMessage = map extractString
        isSevereError (LogMessage (Error s) _ _) = s >= 50
        isSevereError _ = False
        extractString (LogMessage _ _ s) = s
        extractString (Unknown s) = s

-- Exercise 6
-- Solution from https://github.com/elbeno/cis194/blob/master/homework2/LogAnalysis.hs#L70

culprit :: IO String
culprit = do
  errs <- testWhatWentWrong parse whatWentWrong "error.log"
  return $ map head errs
