{-# LANGUAGE DeriveFunctor #-}

module Day04
  ( solve
  ) where

import Data.List (transpose, inits)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type BingoSeq   = [Int]
type BingoBoard = [Int]

-- get list of rows of board
boardRows :: BingoBoard -> [[Int]]
boardRows []  = []
boardRows bs  = take 5 bs : boardRows (drop 5 bs)

-- get list of columns of board
boardCols :: BingoBoard -> [[Int]]
boardCols = transpose . boardRows

-- check if board wins with sequence
boardWins :: BingoSeq -> BingoBoard -> Bool
boardWins xs board    = any allmarked racs
  where racs          = boardRows board ++ boardCols board
        allmarked ns  = all (`elem` xs) ns

-- get unmarked numbers on board for sequence
unmarked :: BingoSeq -> BingoBoard -> [Int]
unmarked xs = filter (`notElem` xs)

-- Parsing --

parseInt :: Parser Int
parseInt = do
  num <- P.many1 P.digit
  return $ read num

parseSequence :: Parser BingoSeq
parseSequence = P.sepBy parseInt (P.char ',')

parseBoard :: Parser BingoBoard
parseBoard = do
  rows <- P.count 5 parseRow
  return $ concat rows
  where parseRow = P.count 5 (parseInt <* P.spaces)

parseInput :: Parser (BingoSeq, [BingoBoard])
parseInput = do
  bingoseq <- parseSequence
  P.spaces
  boards <- P.sepBy parseBoard P.spaces
  return (bingoseq, boards)

-- Part 1 --

-- get first winning board and the sequence that won it
firstWinner :: [BingoSeq] -> [BingoBoard] -> (BingoSeq, BingoBoard)
firstWinner [] _              = error "No sequences"
firstWinner _ []              = error "No boards"
firstWinner (bs:bss) boards
  | any (boardWins bs) boards = (bs, head $ filter (boardWins bs) boards)
  | otherwise                 = firstWinner bss boards

part1 :: String -> IO ()
part1 input = do
  let Right (bingoseq, boards) = P.parse parseInput "(input)" input
  let (winseq, winboard) = firstWinner (inits bingoseq) boards
  print $ sum (unmarked winseq winboard) * last winseq

-- Part 2 --

-- filter out winners for increasing sequences until 1 remains
lastWinner :: [BingoSeq] -> [BingoBoard] -> (BingoSeq, BingoBoard)
lastWinner [] _             = error "No sequences"
lastWinner _ []             = error "No boards"
lastWinner bss [b]          = firstWinner bss [b]
lastWinner (bs:bss) boards  = lastWinner bss $ filter (not . boardWins bs) boards

part2 :: String -> IO ()
part2 input = do
  let Right (bingoseq, boards) = P.parse parseInput "(input)" input
  let winners = filter (boardWins bingoseq) boards  -- discard boards that never win
  let (winseq, winboard) = lastWinner (inits bingoseq) winners
  print $ sum (unmarked winseq winboard) * last winseq

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
