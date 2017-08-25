{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Ex 1
getErrorInt :: String -> Int
getErrorInt m = read((words m) !! 1) :: Int

extractInt :: Int -> String -> Int
extractInt i m = read((words m) !! i) :: Int

getTimeStamp :: MessageType -> String -> Int
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
  | head m == 'E' = LogMessage (Error (getErrorInt m)) (getTimeStamp (Error 7) m) (getMessage (Error 7) m)
  | otherwise = Unknown m

-- parse :: String -> [LogMessage]