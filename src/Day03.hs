module Day03
  ( solve
  ) where

import Data.List (transpose)

-- Part 1 --

-- get the char that's most common ('0' or '1')
mostCommon :: String -> Char
mostCommon s
  | zeros > ones  = '0'
  | ones > zeros  = '1'
  | otherwise     = error "Equal amount of 0s and 1s"
  where zeros = length $ filter (=='0') s
        ones  = length $ filter (=='1') s

-- convert string of 0s and 1s representing a binary value to int
binToInt :: String -> Int
binToInt s = sum $ map (\(i,v) -> v * 2^i) indexed
  where indexed = zip [0..] $ map (\c -> if c == '0' then 0 else 1) $ reverse s

-- invert string of 0s and 1s
binInvert :: String -> String
binInvert []        = ""
binInvert ('0':xs)  = '1' : binInvert xs
binInvert ('1':xs)  = '0' : binInvert xs
binInvert _         = error "Error inverting"

part1 :: String -> IO ()
part1 input = do
  let mosts   = map mostCommon $ transpose $ lines input
  let gamma   = binToInt mosts
  let epsilon = binToInt $ binInvert mosts
  print $ gamma * epsilon

-- Part 2 --

-- position of bit to consider, and list of string representations of binary numbers
-- filter the list according to criteria, until the list only has 1 item
oxygenRating :: Int -> [String] -> [String]
oxygenRating i ss
  | length ss == 1  = ss
  | otherwise       = oxygenRating (i+1) $ filter (\s -> s!!i == most) ss
  where bits    = map (!!i) ss
        zeros   = length $ filter (=='0') bits
        ones    = length $ filter (=='1') bits
        most    | zeros > ones  = '0'
                | otherwise     = '1'

-- position of bit to consider, and list of string representations of binary numbers
-- filter the list according to criteria, until the list only has 1 item
co2Rating :: Int -> [String] -> [String]
co2Rating i ss
  | length ss == 1  = ss
  | otherwise       = co2Rating (i+1) $ filter (\s -> s!!i == least) ss
  where bits    = map (!!i) ss
        zeros   = length $ filter (=='0') bits
        ones    = length $ filter (=='1') bits
        least   | ones < zeros  = '1'
                | otherwise     = '0'

part2 :: String -> IO ()
part2 input = do
  let oxygen  = binToInt $ head $ oxygenRating 0 $ lines input
  let co2     = binToInt $ head $ co2Rating 0 $ lines input
  print $ oxygen * co2

-- Main --

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
