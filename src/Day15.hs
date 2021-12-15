module Day15
  ( solve
  ) where

-- Dijkstra

import Helpers
import Data.Char (ord)
import qualified Data.Map.Strict as M

type Coord  = (Int,Int)
data Node   = Node
  { distance  :: Int
  , previous  :: Maybe Coord
  , value     :: Int
  } deriving (Show)
type NodeMap  = M.Map Coord Node

type Parsed = [[Int]]

parseInput :: String -> Parsed
parseInput input = map readLine $ lines input
  where readLine []     = []
        readLine (x:xs) = (ord x - 48) : readLine xs

getDistance :: Parsed -> Coord -> Int
getDistance parsed (x,y) = parsed!!y!!x

maxX :: Parsed -> Int
maxX input = length (head input) - 1

maxY :: Parsed -> Int
maxY input = length input - 1

dijkstra' :: (NodeMap, NodeMap) -> Coord -> (NodeMap, NodeMap)
dijkstra' (visited,unvisited) visiting@(x,y)
  | M.null unvisited' = (visited',mempty)
  | otherwise         = dijkstra' (visited', unvisited') next
  where neighbours          = filter (`M.member` unvisited) [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]
        Just visitnode      = M.lookup visiting unvisited
        visitdist           = distance visitnode
        updateNeighbour m n = M.update f n m
        f orig@(Node d p v) = if visitdist + v < d then Just (Node (visitdist+v) (Just visiting) v) else Just orig
        unvisited'          = M.delete visiting $ foldl updateNeighbour unvisited neighbours
        visited'            = M.insert visiting visitnode visited
        next                = fst $ head $ sortOn (distance . snd) $ M.toList unvisited'

dijkstra :: NodeMap -> NodeMap
dijkstra input = fst $ dijkstra' (mempty,input) (0,0)

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
  print 0
  {-
  case M.lookup (maxX exploded, maxY exploded) solved of
    Nothing -> print "Impossible!"
    Just n  -> print $ distance n
    -}

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
