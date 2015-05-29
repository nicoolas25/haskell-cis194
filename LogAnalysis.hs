{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import qualified Data.String as Str

parseMessage :: String -> LogMessage
parseMessage string = case Str.words string of
  "E":s:ts:m -> LogMessage (Error (read s)) (read ts) (Str.unwords m)
  "I":ts:m -> LogMessage Info (read ts) (Str.unwords m)
  "W":ts:m -> LogMessage Warning (read ts) (Str.unwords m)
  _ -> Unknown string

parse :: String -> [LogMessage]
parse = map parseMessage . lines
