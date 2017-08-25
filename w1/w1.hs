{-
  Key points:
  Functions are values, and can be used in exactly the same ways as any other sort of value.
  Evaluating expressions rather than executing instructions.
-}

-- :: read as "has type", a function is a value, a value has a type
foo :: Int -> Int
-- function defintion => expressions and pattern matching
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n +3

hailstone :: Int -> Int
hailstone n | mod n 2 == 0 = div n 2 | otherwise = 3 * n + 1

hailstoneSeq :: Int -> [Int]
hailstoneSeq 0 = []
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq(hailstone n)

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

hailstoneLen :: Int -> Int
hailstoneLen n = intListLength (hailstoneSeq n)


-- Ex 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev(div x 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse(doubleEveryOtherL2R(reverse(x)))

doubleEveryOtherL2R :: [Integer] -> [Integer]
doubleEveryOtherL2R [] = []
doubleEveryOtherL2R (x:[]) = [x]
doubleEveryOtherL2R (x:y:arr) = x:2 * y:doubleEveryOtherL2R(arr)

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits(x)) + sumDigits(xs)

-- Ex 4
validate :: Integer -> Bool
validate n
  | n <=0 = False
  | otherwise = if (sumDigits(doubleEveryOther(toDigits(n))) `mod` 10) == 0 then True else False

-- Ex 5
-- s: source, t: temporary, d: destination
-- from zero, to one, two, three, and then formulate an expression
-- recursive solution reference: https://en.wikipedia.org/wiki/Tower_of_Hanoi
-- total steps = 2^n -1
type Peg = String
type Move = (Integer, Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 s t d = (1, s, d) : hanoi (1-1) s t d
hanoi 2 s t d = hanoi (2-1) s d t ++ (2, s, d) : hanoi (2-1) t s d
hanoi 3 s t d = (hanoi (3-1) s d t) ++ (3, s, d) : hanoi (3-1) t s d
hanoi n s t d = (hanoi (n -1) s d t) ++ (n, s, d) : hanoi (n -1) t s d