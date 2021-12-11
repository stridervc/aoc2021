module Day11
  ( solve
  ) where

import Helpers
import Data.Char (ord)
import qualified Data.Map as M

type Coord  = (Int,Int)
type Parsed = M.Map Coord (Int,Bool)

parseInput :: String -> Parsed
parseInput input = M.fromList $ concatMap (uncurry parseLine) numberedLines
  where charToInt c     = ord c - 48
        numberedLines   = zip [0..] $ lines input
        parseLine y l   = zipWith (parseChar y) [0..] l
        parseChar y x c = ((x,y),(charToInt c, False))

-- Part 1 --

incrPos :: Parsed -> Coord -> Parsed
incrPos parsed key = M.adjust (\(v,f) -> (v+1,f)) key parsed

-- if energy > 9 and hasn't flashed, flash it and incr its neighbours, potentially flashing them as well
updatePos :: Parsed -> Coord -> Parsed
updatePos parsed key@(x,y)
  | invalid     = parsed
  | energy <= 9 = parsed
  | flashed     = parsed
  | otherwise   = updateAllPos $ foldl incrPos marked neighbours
  where invalid               = key `M.notMember` parsed
        Just (energy,flashed) = M.lookup key parsed
        marked                = M.adjust (\(v,_) -> (v,True)) key parsed
        neighbours            = [ (x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

updateAllPos :: Parsed -> Parsed
updateAllPos parsed = foldl updatePos parsed $ M.keys parsed

-- incr all energies, flash ones that should, reset flashed ones
-- return new state and number of flashes
step :: (Parsed, Int) -> (Parsed, Int)
step (parsed, _) = (reset, count)
  where incr  = M.map (\(v,f) -> (v+1,f)) parsed
        flash = updateAllPos incr
        count = M.foldl (\c (_,f) -> if f then c+1 else c) 0 flash
        reset = M.map (\(v,f) -> if v > 9 then (0,False) else (v,f)) flash

-- 101 because index 0 is just our input with iterate
part1 :: Parsed -> IO ()
part1 input = print $ sum $ map snd $ take 101 $ iterate step (input, 0)

-- Part 2 --

part2 :: Parsed -> IO ()
part2 input = print $ length $ takeWhile (/=100) $ map snd $ iterate step (input, 0)

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
