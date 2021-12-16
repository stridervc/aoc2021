module Day15
  ( solve
  ) where

-- Dijkstra
-- do some profiling

import Helpers
import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord  = (Int,Int)
data Node   = Node
  { distance  :: Int
  , previous  :: Maybe Coord
  , value     :: Int
  } deriving (Show)
type NodeMap  = M.Map Coord Node
type CoordSet = S.Set Coord

type Parsed = [[Int]]

parseInput :: String -> Parsed
parseInput input = map readLine $ lines input
  where readLine []     = []
        readLine (x:xs) = (ord x - 48) : readLine xs

maxX :: Parsed -> Int
maxX input = length (head input) - 1

maxY :: Parsed -> Int
maxY input = length input - 1

dijkstra' :: (CoordSet, NodeMap) -> Coord -> (CoordSet, NodeMap)
dijkstra' (unvisited,nodes) visiting@(x,y)
  | S.null unvisited' = (mempty, nodes')
  | otherwise         = dijkstra' (unvisited', nodes') $ fst next
  where neighbours          = filter (`S.member` unvisited) [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]
        Just node           = M.lookup visiting nodes
        mydist              = distance node
        updateNode m n      = M.update f n m
        f orig@(Node d p v) = if mydist + v < d then Just (Node (mydist+v) (Just visiting) v) else Just orig
        nodes'              = foldl updateNode nodes neighbours
        unvisited'          = S.delete visiting unvisited
        next                = M.foldrWithKey' (\k n accum -> if distance n < snd accum && S.member k unvisited' then (k,distance n) else accum) ((0,0),maxBound) nodes'

dijkstra :: NodeMap -> NodeMap
dijkstra input = snd $ dijkstra' (S.fromList $ M.keys input, input) (0,0)

inputToMap :: Parsed -> NodeMap
inputToMap input = M.insert (0,0) homeNode $ M.fromList [ ((x,y),unvisitedNode $ value (x,y)) | x <- [0..maxX input], y <- [0..maxY input] ]
  where homeNode      = Node 0 Nothing $ value (0,0)
        value (x,y)   = input!!y!!x
        unvisitedNode = Node maxBound Nothing

-- for debugging
pathTo :: NodeMap -> Coord -> [Coord]
pathTo _ (0,0)  = [(0,0)]
pathTo m dest   = dest : pathTo m prev
  where Just node = M.lookup dest m
        Just prev = previous node

-- Part 1 --

part1 :: Parsed -> IO ()
part1 input = do
  let solved = dijkstra $ inputToMap input
  case M.lookup (maxX input, maxY input) solved of
    Nothing -> print "Impossible!"
    Just n  -> print $ distance n

-- Part 2 --

explodeInput :: Parsed -> Parsed
explodeInput input = map buildrow [0..h*5-1]
  where buildrow y  = [ value (x,y) | x <- [0..w*5-1] ]
        adjust 9    = 1
        adjust x    = x+1
        w           = maxX input + 1
        h           = maxY input + 1
        value (x,y) | x < w && y < h  = input!!y!!x
                    | x >= w && y < h = adjust $ value (x-w,y)
                    | x < w && y >= h = adjust $ value (x,y-h)
                    | otherwise       = adjust $ adjust $ value (x-w,y-h)

part2 :: Parsed -> IO ()
part2 input = do
  let exploded  = explodeInput input
  let solved    = dijkstra $ inputToMap exploded
  case M.lookup (maxX exploded, maxY exploded) solved of
    Nothing -> print "Impossible!"
    Just n  -> print $ distance n

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
