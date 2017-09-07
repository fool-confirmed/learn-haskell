module Golf where

-- Ex. 1
skips :: [a] -> [[a]]
skips [] = []
skips xs = shrink xs 0

shrink :: [a] -> Int -> [[a]]
shrink [] _ = []
shrink xs i = picky xs i : shrink (tail xs) (i + 1)

picky :: [a] -> Int -> [a]
picky [] _ = []
picky xs s 
  | s == 0 = xs
  | otherwise = head xs : picky (drop (s + 1) xs) s  
