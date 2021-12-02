module Day02
  ( solve
  ) where

data Command = Forward Int | Down Int | Up Int | InvalidCommand String deriving (Eq, Show)

data Submarine = Submarine
  { horisontalPos :: Int
  , depth         :: Int
  , aim           :: Int
  } deriving (Eq, Show)

newSubmarine :: Submarine
newSubmarine = Submarine 0 0 0

perform :: Submarine -> Command -> Submarine
perform s (Forward x)         = s { horisontalPos = horisontalPos s + x }
perform s (Down    x)         = s { depth         = depth s + x }
perform s (Up      x)         = s { depth         = depth s - x }
perform s (InvalidCommand e)  = error e

performB :: Submarine -> Command -> Submarine
performB s (Forward x)        = s { horisontalPos = horisontalPos s + x, depth = depth s + aim s * x }
performB s (Down    x)        = s { aim           = aim s + x }
performB s (Up      x)        = s { aim           = aim s - x }
performB s (InvalidCommand e) = error e

parseCommand :: String -> Command
parseCommand str = case v of
              "forward" -> Forward x
              "down"    -> Down x
              "up"      -> Up x
              _         -> InvalidCommand str
  where v = head $ words str
        x = read $ words str !! 1

parseInput = map parseCommand . lines

part1 :: String -> IO ()
part1 input = do
  let submarine = foldl perform newSubmarine $ parseInput input
  print $ horisontalPos submarine * depth submarine

part2 :: String -> IO ()
part2 input = do
  let submarine = foldl performB newSubmarine $ parseInput input
  print $ horisontalPos submarine * depth submarine

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 input
  putStr "Part 2 : "
  part2 input
