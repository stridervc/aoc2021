module Day10
  ( solve
  ) where

import Helpers

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = lines

-- Part 1 --

score1 :: Char -> Int
score1 ')'  = 3
score1 ']'  = 57
score1 '}'  = 1197
score1 '>'  = 25137
score1 _    = 0

type Stack  = [Char]

scoreLine1 :: Stack -> String -> Int
scoreLine1 _ []  = 0
scoreLine1 stack (c:cs)
  | opens     = scoreLine1 pushed cs
  | matches   = scoreLine1 popped cs
  | otherwise = score1 c
  where opens   = c `elem` "([{<"
        top     = head stack
        popped  = tail stack
        pushed  = c:stack
        matches | c == ')'  = top == '('
                | c == ']'  = top == '['
                | c == '}'  = top == '{'
                | c == '>'  = top == '<'
                | otherwise = error "Error in 'scoreLine.matches'"

part1 :: Parsed -> IO ()
part1 input = print $ sum $ map (scoreLine1 []) input

-- Part 2 --

score2 :: Char -> Int
score2 ')'  = 1
score2 ']'  = 2
score2 '}'  = 3
score2 '>'  = 4
score2 _    = 0

completeLine :: Stack -> String -> String
completeLine [] []  = ""
completeLine (sc:scs) []
  | sc == '(' = ')' : completeLine scs []
  | sc == '[' = ']' : completeLine scs []
  | sc == '{' = '}' : completeLine scs []
  | sc == '<' = '>' : completeLine scs []
  | otherwise = error "Error in completeLine"
completeLine stack (c:cs)
  | opens     = completeLine pushed cs
  | otherwise = completeLine popped cs
  where opens   = c `elem` "([{<"
        popped  = tail stack
        pushed  = c:stack

scoreLine2 :: Int -> String -> Int
scoreLine2 score ""     = score
scoreLine2 score (c:cs) = scoreLine2 score' cs
  where score'  = score * 5 + score2 c

part2 :: Parsed -> IO ()
part2 input = do
  let incompletes = filter (\l -> scoreLine1 [] l == 0) input
  let scores      = sort $ map (scoreLine2 0 . completeLine []) incompletes
  print $ scores!!((length scores - 1) `div` 2)

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
