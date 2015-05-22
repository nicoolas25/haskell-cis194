module Homework1 where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n = right : (toDigitsRev remaining)
  where right = n `mod` 10
        remaining = n `div` 10

