{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Ex 1
getErrorInt :: String -> Int
getErrorInt m = read((words m) !! 1) :: Int

extractInt :: Int -> String -> Int
extractInt i m = read((words m) !! i) :: Int

getTimeStamp :: MessageType -> String -> TimeStamp
getTimeStamp t m
  | (t == Info || t == Warning) = extractInt 1 m
  | otherwise = extractInt 2 m

extractString :: Int -> String -> String
extractString i m = unwords(drop i (words m))

getMessage :: MessageType -> String -> String
getMessage t m
  | (t == Info || t == Warning) = extractString 2 m
  | otherwise = extractString 3 m

parseMessage :: String -> LogMessage
parseMessage m 
  | head m == 'I' = LogMessage Info (getTimeStamp Info m) (getMessage Info m)
  | head m == 'W' = LogMessage Warning (getTimeStamp Warning m) (getMessage Warning m)  
  | head m == 'E' = LogMessage (Error (getErrorInt m)) (getTimeStamp (Error (getErrorInt m)) m) (getMessage (Error (getErrorInt m)) m)
  | otherwise = Unknown m

-- Ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node lt m rt) 
  | (lmGetTime lm) <= (lmGetTime m) = Node lt lm (Node Leaf m rt)
  | (lmGetTime lm) > (lmGetTime m) = Node lt m (insert lm rt)
-- lt = leaf lm1 rt, rt = lt lm2 leaf
-- node = leaf/node message leaf/node

lmGetType :: LogMessage -> MessageType
lmGetType (LogMessage tp _ _) = tp

lmGetTime :: LogMessage -> TimeStamp
lmGetTime (LogMessage _ ts _) = ts
lmGetTime (Unknown _) = 0

-- Ex 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node _ m rt) = m : inOrder(rt)

-- Ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((Unknown _):xs) = whatWentWrong xs
whatWentWrong ((LogMessage (Error i) _ m):xs) 
  | i >= 50 = m : whatWentWrong xs
  | otherwise = whatWentWrong xs
whatWentWrong ((LogMessage _ _ _):xs) = whatWentWrong xs

-- test code
foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8, foo9, foo10, foo11 :: LogMessage
foo1 = parseMessage "I 6 Completed armadillo processing"
foo2 = parseMessage "I 1 Nothing to report"
foo3 = parseMessage "E 99 10 Flange failed!"
foo4 = parseMessage "I 4 Everything normal"
foo5 = parseMessage "I 11 Initiating self-destruct sequence"
foo6 = parseMessage "E 70 3 Way too many pickles"
foo7 = parseMessage "E 65 8 Bad pickle-flange interaction detected"
foo8 = parseMessage "W 5 Flange is due for a check-up"
foo9 = parseMessage "I 7 Out for lunch, back in two time steps"
foo10 = parseMessage "E 20 2 Too many pickles"
foo11 = parseMessage "I 9 Back from lunch"


myMT :: MessageTree
myMT = build [foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8, foo9, foo10, foo11]
-- ghci: whatWentWrong (inOrder myMT)
