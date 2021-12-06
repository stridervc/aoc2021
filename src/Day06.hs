module Day06
  ( solve
  ) where

import Helpers

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

-- Part 1 --

advance :: [Int] -> [Int]
advance []      = []
advance (x:xs)
  | x == 0    = [6,8] ++ advance xs
  | otherwise = x - 1 : advance xs

-- apply f x number of times to a
applyX :: Int -> (a -> a) -> a -> a
applyX 0 _ a = a
applyX x f a = applyX (x-1) f (f a)

part1 :: String -> IO ()
part1 input = do
  print $ length $ applyX 80 advance $ parseInput input

-- Part 2 --

type Timer = Int

-- count how many fish there are with each time remaining (0 to 8)
countTimers :: [Timer] -> [(Timer,Int)]
countTimers fish = map (\t -> (t, count t)) [0..8]
  where count t = length $ filter (==t) fish

advanceTimers :: [(Timer,Int)] -> [(Timer,Int)]
advanceTimers tfs = map (\t -> (t, count (t+1))) [0..5] ++ [(6,count 7 + count 0), (7,count 8), (8,count 0)]
  where count t = snd $ tfs!!t

part2 :: String -> IO ()
part2 input = do
  let advanced = applyX 256 advanceTimers $ countTimers $ parseInput input
  print $ sum $ map snd advanced

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
