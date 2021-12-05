module Day05
  ( solve
  ) where

import Helpers

import Data.List (sort)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

type VentPoint  = (Int,Int)
type VentLine   = (VentPoint,VentPoint)

isStraight :: VentLine -> Bool
isStraight ((x1,y1),(x2,y2))
  | x1 == x2  = True
  | y1 == y2  = True
  | otherwise = False

-- expand vent line to all its points
-- only works for straight and perfectly diagonal lines
expand :: VentLine -> [VentPoint]
expand (p1@(x1,y1),p2@(x2,y2))
  | x1 == x2 && y1 == y2  = [p1]
  | otherwise             = p1 : expand ((nx,ny),p2)
  where nx  | x1 == x2    = x1
            | x1 < x2     = x1 + 1
            | otherwise   = x1 - 1
        ny  | y1 == y2    = y1
            | y1 < y2     = y1 + 1
            | otherwise   = y1 - 1

-- count occurences of point in list
-- list has to be sorted
-- This could have been a map length $ group . sort :(
countOccurences' :: [VentPoint] -> [(Int,VentPoint)]
countOccurences' []       = []
countOccurences' (vp:vps) = (n,vp) : countOccurences' vps'
  where n     = 1 + length (takeWhile (==vp) vps)
        vps'  = drop (n-1) vps

countOccurences = countOccurences' . sort

-- Parsing --

parseVentLine :: Parser VentLine
parseVentLine = do
  x1 <- parseInt
  P.char ','
  y1 <- parseInt
  P.string " -> "
  x2 <- parseInt
  P.char ','
  y2 <- parseInt
  return ((x1,y1),(x2,y2))

parseInput :: Parser [VentLine]
parseInput = P.endBy parseVentLine (P.spaces <|> P.eof)

parse = P.parse parseInput "(input)"

-- Part 1 --

part1 :: String -> IO ()
part1 input = do
  let Right ventlines = parse input
  let points = concatMap expand $ filter isStraight ventlines
  print $ length $ filter (>=2) $ map fst $ countOccurences points

-- Part 2 --

part2 :: String -> IO ()
part2 input = do
  let Right ventlines = parse input
  let points = concatMap expand ventlines
  print $ length $ filter (>=2) $ map fst $ countOccurences points

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
