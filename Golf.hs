module Golf where

-- Exercise 1

everyNth :: [a] -> Int -> [a]
everyNth l n = [ snd x | x <- zip (cycle [1..n]) l, fst x == n ]

skips :: [a] -> [[a]]
skips l = map (everyNth l) [1..length l]

-- Exercise 2



-- Exercise 3

