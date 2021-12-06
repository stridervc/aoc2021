module Day06
  ( solve
  ) where

import Helpers

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

-- count how many fish there are with each time remaining (0 to 8)
countTimers :: [Int] -> [Int]
countTimers fish = map count [0..8]
  where count t = length $ filter (==t) fish

advanceTimers :: [Int] -> [Int]
advanceTimers tfs = map (\t -> count (t+1)) [0..5] ++ [count 7 + count 0, count 8, count 0]
  where count t = tfs!!t

solve :: String -> IO ()
solve input = do
  let advanced = iterate advanceTimers $ countTimers $ parseInput input
  putStr "Part 1 : "
  print $ sum $ advanced!!80
  putStr "Part 2 : "
  print $ sum $ advanced!!256
