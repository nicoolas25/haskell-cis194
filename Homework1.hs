module Homework1 where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n = right : (toDigitsRev remaining)
  where right = n `mod` 10
        remaining = n `div` 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = [ if i `mod` 2 == 0 then 2 * item else item | (item, i) <- lWithIndex ]
  where lWithIndex = zip l indexes
        indexes = [startFrom..]
        startFrom = (length l) `mod` 2

sumDigits :: [Integer] -> Integer
sumDigits = sum . (map (sum . toDigitsRev))
