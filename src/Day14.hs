module Day14
  ( solve
  ) where

import Helpers
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import Data.List (group)
import qualified Data.Map.Strict as M

type Polymer    = String
type Pair       = (Char,Char)
type Pairs      = [Pair]
type Rule       = (Pair,Char)
type Rules      = [Rule]
type PairCounts = M.Map Pair Int

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

type Parsed = (PairCounts,Rules)

addPair :: Int -> PairCounts -> Pair -> PairCounts
addPair n pcs pair = M.alter f pair pcs
  where f Nothing   = Just n
        f (Just c)  = Just $ n + c

parseInput :: String -> Parsed
parseInput input = do
  let Right (polymer,rules) = P.parse parseAll "(input)" input
  let paired      = [ (polymer!!(i-1),polymer!!i) | i <- [1..length polymer - 1] ]
  let countpairs  = foldl (addPair 1) mempty paired
  (countpairs, rules)

-- a pair will split into two pairs, if there's a rule that matches the pair
splitPair :: Rules -> Pair -> [Pair]
splitPair rules pair@(c1,c2)
  | null lookupRule = [pair]
  | otherwise       = [pair1, pair2]
  where lookupRule  = head $ filter (\r -> fst r == pair) rules
        pair1       = (c1, snd lookupRule)
        pair2       = (snd lookupRule, c2)

-- perform one set of insertions
step :: Rules -> PairCounts -> PairCounts
step rules = M.foldlWithKey f mempty
  where f newpcs k c  = foldl (addPair c) newpcs $ splitPair rules k

stepN :: Int -> Rules -> PairCounts -> PairCounts
stepN n rules pcs = last $ take n $ tail $ iterate (step rules) pcs

countChars :: PairCounts -> [(Char,Int)]
countChars pcs = [ (ch,(countat fst ch + countat snd ch) `div` 2) | ch <- allchars ]
  where keys          = M.keys pcs
        allchars      = nub $ map fst keys <> map snd keys
        countat f ch  = sum $ map (\(k,v) -> if f k == ch then v else 0) $ M.toList pcs

-- Part 1 --

part1 :: Parsed -> IO ()
part1 (countpairs,rules) = do
  let stepped = stepN 10 rules countpairs
  let counts = sort $ map snd $ countChars stepped
  print $ last counts - head counts

-- Part 2 --

part2 :: Parsed -> IO ()
part2 (countpairs,rules) = do
  let stepped = stepN 40 rules countpairs
  let counts = sort $ map snd $ countChars stepped
  print $ last counts - head counts

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
