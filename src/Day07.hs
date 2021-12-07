module Day07
  ( solve
  ) where

import Helpers
import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

-- Part 1 --

fuelForPos :: [Int] -> Int -> Int
fuelForPos crabs pos = sum $ map (\c -> abs (pos-c)) crabs

-- Part 2 --

fuelForPos2 :: [Int] -> Int -> Int
fuelForPos2 crabs pos = sum $ map cost crabs
  where d c     = abs $ pos-c
        cost c  = sum $ take (d c) [1..]

solve :: String -> IO ()
solve input = do
  let input'  = parseInput input
  let min'    = minimum input'
  let max'    = maximum input'

  putStr "Part 1 : "
  let fuels = map (fuelForPos input') [min'..max']
  print $ snd $ head $ sortOn snd $ zip [min'..max'] fuels

  putStr "Part 2 : "
  let fuels2 = map (fuelForPos2 input') [min'..max']
  print $ snd $ head $ sortOn snd $ zip [min'..max'] fuels2
