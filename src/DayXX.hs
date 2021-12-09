module DayXX
  ( solve
  ) where

import Helpers

type Input = String

parseInput :: String -> Input
parseInput input = input

-- Part 1 --

part1 :: Input -> IO ()
part1 input = putStrLn "Coming soon..."

-- Part 2 --

part2 :: Input -> IO ()
part2 input = putStrLn "Coming soon..."

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
