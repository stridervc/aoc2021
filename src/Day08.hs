module Day08
  ( solve
  ) where

import Helpers
import qualified Data.Map.Strict as M

parseInput :: String -> [([String],[String])]
parseInput input = map parseLine $ lines input
  where parseLine l = (words (takeWhile (/='|') l), words $ tail (dropWhile (/='|') l))

-- Part 1 --

extract1478 :: [String] -> [String]
extract1478 []  = []
extract1478 (x:xs)
  | l == 2    = x : extract1478 xs
  | l == 4    = x : extract1478 xs
  | l == 3    = x : extract1478 xs
  | l == 7    = x : extract1478 xs
  | otherwise = extract1478 xs
  where l = length x

part1 :: String -> IO ()
part1 input = print $ sum $ map (length . extract1478 . snd) $ parseInput input

-- Part 2 --

-- map of string sequence and the int it represents
type Known = M.Map String Int

-- true iff all elements of xs are contained in ys
containedIn :: String -> String -> Bool
containedIn [] _ = True
containedIn (x:xs) ys
  | x `elem` ys = containedIn xs ys
  | otherwise   = False

-- deduce which number a string represents
sherlock :: [String] -> Known
sherlock xs =
  M.insert one 1 $                  -- length 2 has to be '1'
  M.insert four 4 $                 -- length 4 has to be '4'
  M.insert (head $ lengthOf 3) 7 $  -- length 3 has to be '7'
  M.insert (head $ lengthOf 7) 8 $  -- length 7 has to be '8'
  -- there are 3 length 6s, '0', '6', '9'
  M.insert six 6 $                  -- '6' is the only one that doesn't contain '1'
  M.insert nine 9 $                 -- '9' is the only length 6 that contains '4'
  M.insert zero 0 $                 -- the remaining length 6 is '0'
  -- there are 3 length 5s, '2', '3' and '5'
  M.insert three 3 $                -- '3' is the only one that contains '1'
  M.insert five 5 $                 -- between '2', '3' and '5', only '5' is contained in '6'
  M.insert two 2 mempty             -- the remaining length 5 (not '3' or '5') is '2'
  where lengthOf n  = filter (\x -> length x == n) xs'
        xs'         = map sort xs
        one         = head $ lengthOf 2
        six         = head $ filter (not . containedIn one) $ lengthOf 6
        five        = head $ filter (`containedIn` six) $ lengthOf 5
        three       = head $ filter (containedIn one) $ lengthOf 5
        two         = head $ filter (/= three) $ filter (/= five) $ lengthOf 5
        four        = head $ lengthOf 4
        nine        = head $ filter (containedIn four) $ lengthOf 6
        zero        = head $ filter (/= six) $ filter (/= nine) $ lengthOf 6

lookupKnown :: Known -> String -> Int
lookupKnown known k = case M.lookup (sort k) known of
  Nothing -> error "Oops!"
  Just v  -> v

listToInt' :: Int -> [Int] -> Int
listToInt' _ []       = 0
listToInt' pos (x:xs) = x * 10^pos + listToInt' (pos+1) xs

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt xs = listToInt' 0 $ reverse xs

solveLine :: ([String],[String]) -> Int
solveLine l = listToInt $ map (lookupKnown known) $ snd l
  where known = sherlock $ uncurry (<>) l

part2 :: String -> IO ()
part2 input = print $ sum $ map solveLine $ parseInput input

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
