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

-- apply f, x number of times to a
applyX :: Int -> (a -> a) -> a -> a
applyX 0 _ a = a
applyX x f a = applyX (x-1) f (f a)

-- Part 1 --

part1 :: String -> IO ()
part1 input = do
  let advanced = applyX 80 advanceTimers $ countTimers $ parseInput input
  print $ sum advanced

-- Part 2 --

part2 :: String -> IO ()
part2 input = do
  let advanced = applyX 256 advanceTimers $ countTimers $ parseInput input
  print $ sum advanced

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
