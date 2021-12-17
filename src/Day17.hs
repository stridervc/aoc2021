module Day17
  ( solve
  ) where

import Helpers
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import Data.List (group)
import Data.Function ((&))

type Coord  = (Int, Int)
type Target = (Coord, Coord)

type Parsed = Target

-- hacky but quick
-- negative numbers should not be parsed like this
parseAll :: Parser Target
parseAll = do
  P.string "target area: x="
  x1 <- parseInt
  P.string ".."
  x2 <- parseInt
  P.char ','
  P.spaces
  P.string "y=-"
  y1 <- parseInt
  P.string "..-"
  y2 <- parseInt
  return ((x1,-y1), (x2,-y2))

parseInput :: String -> Parsed
parseInput input = do
  let Right parsed = P.parse parseAll "(input)" input
  parsed

hitsTarget' :: Coord -> Target -> (Int, Int) -> Bool
hitsTarget' (x,y) t@((tx1,ty1),(tx2,ty2)) (dx, dy)
  | hit                 = True
  | x > tx2             = False
  | y < ty1             = False
  | x < tx1 && dx == 0  = False
  | dx == 0             = hitsTarget' (x', y') t (0, dy')
  | otherwise           = hitsTarget' (x', y') t (dx', dy')
  where hit = x >= tx1 && x <= tx2 && y >= ty1 && y <= ty2
        x'  = x + dx
        y'  = y + dy
        dx' = dx - 1
        dy' = dy - 1

hitsTarget = hitsTarget' (0,0)

maxHeight' :: Coord -> (Int, Int) -> Int
maxHeight' (x,y) (dx,dy)
  | y' < y    = y
  | otherwise = maxHeight' (x,y') (dx,dy')
    where y'  = y + dy
          dy' = dy - 1

maxHeight = maxHeight' (0,0)

-- Part 1 --

part1 :: Parsed -> IO ()
part1 target = do
  let ((tx1, ty1), (tx2, ty2)) = target
  -- 10*dx is a guess for the max dy, there should be a way to calculate it based on dx
  -- but this works
  print $ maximum $ map maxHeight $ filter (hitsTarget target) [ (dx,dy) | dx <- [1..tx2+1], dy <- [ty1,ty1+1..10*dx] ]

-- Part 2 --

part2 :: Parsed -> IO ()
part2 target = do
  let ((tx1, ty1), (tx2, ty2)) = target
  -- 10*dx is a guess for the max dy, there should be a way to calculate it based on dx
  -- but this works
  print $ length $ filter (hitsTarget target) [ (dx,dy) | dx <- [1..tx2+1], dy <- [ty1,ty1+1..10*dx] ]

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
