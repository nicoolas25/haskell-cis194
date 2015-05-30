module Golf where

import Data.List

-- Exercise 1

everyNth :: [a] -> Int -> [a]
everyNth l n = [ snd x | x <- zip (cycle [1..n]) l, fst x == n ]

skips :: [a] -> [[a]]
skips l = map (everyNth l) [1..length l]

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r) = if y > x && y > z then y:localMaxima (z:r) else localMaxima (y:z:r)
localMaxima _ = []

-- Exercise 3

frequencies :: [Integer] -> [(Integer, Int)]
frequencies l = [ (i, length r) | (i:r) <- group $ sort $ l ++ [0..9] ]

column :: Int -> (Integer, Int) -> String
column m (i, n) = replicate (m-n) ' ' ++ replicate n '*' ++ "=" ++ show i

histogram :: [Integer] -> String
histogram l = unlines $ transpose $ map (column max) freqs
  where max = maximum $ map snd freqs
        freqs = frequencies l
