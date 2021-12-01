module Day01
  ( solve
  ) where

countIncreases :: Int -> [Int] -> Int
countIncreases start all@(a:b:_)
  | b > a     = countIncreases (start+1) xs
  | otherwise = countIncreases start xs
  where xs    = tail all
countIncreases start _  = start

part1 :: String -> IO ()
part1 input = print $ countIncreases 0 $ map read $ lines input

-- group into sets of 3
windowify :: [Int] -> [[Int]]
windowify xs
  | length xs < 3 = [[]]
  | otherwise     = take 3 xs : windowify (tail xs)

part2 :: String -> IO ()
part2 input = print $ countIncreases 0 $ map sum $ windowify $ map read $ lines input

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
