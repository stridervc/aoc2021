module Day12
  ( solve
  ) where

import Helpers

type Parsed = String

parseInput :: String -> Parsed
parseInput input = input

-- Part 1 --

part1 :: Parsed -> IO ()
part1 input = putStrLn "Coming soon..."

-- Part 2 --

part2 :: Parsed -> IO ()
part2 input = putStrLn "Coming soon..."

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input