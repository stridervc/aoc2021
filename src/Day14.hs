module Day14
  ( solve
  ) where

import Helpers
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import Data.List (group)

type Polymer  = String
type Pair     = (Char,Char)
type Rule     = (Pair,Char)

parsePolymer :: Parser Polymer
parsePolymer = P.many1 P.letter

parseRule :: Parser Rule
parseRule = do
  c1 <- P.letter
  c2 <- P.letter
  P.string " -> "
  c3 <- P.letter
  return ((c1,c2),c3)

parseAll :: Parser (Polymer,[Rule])
parseAll = do
  polymer <- parsePolymer
  P.newline
  P.newline
  rules <- P.endBy parseRule (P.spaces <|> P.eof)
  return (polymer,rules)

type Parsed = (Polymer,[Rule])

parseInput :: String -> Parsed
parseInput input = do
  let Right parsed = P.parse parseAll "(input)" input
  parsed

-- perform insertion of a single pair
performInsert :: [Rule] -> Pair -> String
performInsert rules pair@(c1,c2)
  | null lookupRule = [c1,c2]
  | otherwise       = [c1, snd lookupRule, c2]
  where lookupRule  = head $ filter (\r -> fst r == pair) rules

-- perform one set of insertions
step :: [Rule] -> Polymer -> Polymer
step rules polymer = concatMap init inserted ++ [last (last inserted)]
  where pairs     = [ (polymer!!(i-1),polymer!!i) | i <- [1..length polymer - 1] ]
        inserted  = map (performInsert rules) pairs

stepN :: Int -> [Rule] -> Polymer -> Polymer
stepN n rules polymer = last $ take n $ tail $ iterate (step rules) polymer

-- Part 1 --

-- 3408 in 0m2.939s
part1 :: Parsed -> IO ()
part1 (polymer,rules) = do
  let counts = map length $ group $ sort $ stepN 10 rules polymer
  print $ maximum counts - minimum counts

-- Part 2 --

part2 :: Parsed -> IO ()
part2 (polymer,rules) = putStrLn "Coming soon.."

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
