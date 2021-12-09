module Day09
  ( solve
  ) where

import Helpers
import Data.Char (ord)

parseInput :: String -> [[Int]]
parseInput input = map parseLine $ lines input
  where parseLine ""      = []
        parseLine (x:xs)  = (ord x - 48) : parseLine xs

val :: [[Int]] -> (Int,Int) -> Int
val vals (x,y) = row!!x
  where row = vals!!y

neighbours :: [[Int]] -> (Int,Int) -> [(Int,Int)]
neighbours vals (x,y)
  | x == 0 && y == 0        = [east, south]
  | x == 0 && y == maxy     = [east, north]
  | x == 0                  = [east, north, south]
  | x == maxx && y == 0     = [west, south]
  | x == maxx && y == maxy  = [west, north]
  | x == maxx               = [west, north, south]
  | y == 0                  = [east, west, south]
  | y == maxy               = [east, west, north]
  | otherwise               = [north, east, south, west]
  where maxx  = length (head vals) - 1
        maxy  = length vals - 1
        north = (x,y-1)
        south = (x,y+1)
        east  = (x+1,y)
        west  = (x-1,y)

isLowest :: [[Int]] -> (Int,Int) -> Bool
isLowest vals pos = val' < minimum (map (val vals) $ neighbours vals pos)
  where val'  = val vals pos

lowPoints vals = filter (isLowest vals) [(x,y) | x <- [0..maxx], y <- [0..maxy]]
  where maxx  = length (head vals) - 1
        maxy  = length vals - 1

-- Part 1 --

part1 :: String -> IO ()
part1 input = do
  let parsed = parseInput input
  print $ sum $ map ((+1) . val parsed) $ lowPoints parsed

-- Part 2 --

basinNeighbours :: [[Int]] -> (Int,Int) -> [(Int,Int)]
basinNeighbours vals pos  = nub $ directNeighbours <> concatMap (basinNeighbours vals) directNeighbours
  where directNeighbours  = filter (\pos -> val vals pos /= 9 && val vals pos > val') $ neighbours vals pos
        val'              = val vals pos

basinSize :: [[Int]] -> (Int,Int) -> Int
basinSize vals pos = 1 + length (basinNeighbours vals pos)

part2 :: String -> IO ()
part2 input = do
  let parsed  = parseInput input
  let lows    = lowPoints parsed
  print $ product $ take 3 $ reverse $ sort $ map (basinSize parsed) lows

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
