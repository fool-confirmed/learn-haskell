module Golf where

import Data.List

-- Ex. 1
-- must hard code 0 to call shrink
skips :: [a] -> [[a]]
skips [] = []
skips xs = shrink xs 0

-- make a list like "abcd" to a list of sub-list ["abcd", "bcd", "cd", "d"]
-- then use picky function to present the picked elements
shrink :: [a] -> Int -> [[a]]
shrink [] _ = []
shrink xs i = picky xs i : shrink (tail xs) (i + 1)

-- pick elements from a list
-- 0 = everything
-- n = first + jump n position and repeat
picky :: [a] -> Int -> [a]
picky [] _ = []
picky xs s 
  | s == 0 = xs
  | otherwise = head xs : picky (drop (s + 1) xs) s  

-- Ex. 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima lst = filter (\x -> x > 0) (checkThree lst)

-- pattern: process three, then trim off the first, then repeat
checkThree :: [Integer] -> [Integer]
checkThree [] = []
checkThree lst
  | length lst >= 3 = findMiddle (lst !! 0) (lst !! 1) (lst !! 2) : checkThree (tail lst)
  | otherwise = []

-- given three numbers, if the middle is the biggest then return it
-- otherwise -1
findMiddle :: Integer -> Integer -> Integer -> Integer
findMiddle a b c = if a < b && b > c then b else -1

-- Ex. 3
myLst = replicate 9 ""
histogram :: [Int] -> String
histogram [] = ""
histogram xs = simpleStar xs ++ "\n" ++ replicate 10 '=' ++ "\n" ++ "0123456789\n"

simpleStar :: [Int] -> String
simpleStar [] = ""
simpleStar (x:xs) = (replicate x ' ') ++ "*" ++ (replicate (9 - x) ' ') ++ "\n" ++ simpleStar xs

-- in progress