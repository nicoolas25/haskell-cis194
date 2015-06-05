module Homework4 where

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate serie
  where serie n = if even n then n `div` 2 else 3 * n + 1

-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node h left b right)
    | hLeft < hRight || spaceTree left = Node h (insertTree a left) b right
    | hLeft > hRight || spaceTree right = Node h left b (insertTree a right)
    | otherwise = Node (h+1) (insertTree a left) b right
  where hLeft      = heightTree left
        hRight     = heightTree right

heightTree :: Tree a -> Integer
heightTree Leaf = -1
heightTree (Node n _ _ _) = n

spaceTree :: Tree a -> Bool
spaceTree node = case node of
  Leaf -> False
  (Node _ Leaf _ Leaf) -> False
  (Node _ Leaf _ _) -> True
  (Node _ _ _ Leaf) -> True
  (Node _ nodeLeft _ nodeRight) -> spaceTree nodeLeft || spaceTree nodeRight

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


