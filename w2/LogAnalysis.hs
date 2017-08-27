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
insert lm Leaf = Node Leaf lm Leaf
insert (Unknown _) mt = mt
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

foo1, foo2 :: LogMessage
foo1 = parseMessage "E 100 123 error foo"
foo2 = parseMessage "I 234 information foo"
foo3 = parseMessage "this is an unknow message"

myMT :: MessageTree
myMT = Node (Node Leaf foo1 Leaf) foo2 Leaf


-- parse :: String -> [LogMessage]