module Day11
  ( solve
  ) where

import Helpers

type Parsed = String

parseParsed :: String -> Parsed
parseParsed input = input

-- Part 1 --

part1 :: Parsed -> IO ()
part1 input = putStrLn "Coming soon..."

-- Part 2 --

part2 :: Parsed -> IO ()
part2 input = putStrLn "Coming soon..."

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseParsed input
  putStr "Part 2 : "
  part2 $ parseParsed input
