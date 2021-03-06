module Homework1 where

-- Credit card validation

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n = right : (toDigitsRev remaining)
  where right = n `mod` 10
        remaining = n `div` 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse $ doubleOddIndex False $ reverse l
  where doubleOddIndex _ [] = []
        doubleOddIndex True (x:xs) = (x*2):(doubleOddIndex False xs)
        doubleOddIndex False (x:xs) = x:(doubleOddIndex True xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

validate :: Integer -> Bool
validate n = sumOfDigits `mod` 10 == 0
  where sumOfDigits = sumDigits $ doubleEveryOther $ toDigits n

-- Hanoi

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = step1 ++ [(a, b)] ++ step3
  where step1 = hanoi (n-1) a c b
        step3 = hanoi (n-1) c b a

fsa :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
fsa 0 a b c d = []
fsa n a b c d = step1 ++ step2 ++ step3
  where step1 = fsa k a c b d
        step2 = hanoi (n-k) a b d
        step3 = fsa k c b a d
        k = n `div` 2
