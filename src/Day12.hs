module Day12
  ( solve
  ) where

import Helpers
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import Data.Char (isUpper)

data Cave           = CaveStart | CaveEnd | Cave String deriving (Eq, Show, Ord)
newtype Connection  = Connection (Cave,Cave) deriving (Eq, Show)
type Path           = [Cave]
type CaveMap        = M.Map Cave [Cave] -- which caves (values) are connected to which keys

type Parsed = CaveMap

parseCave :: Parser Cave
parseCave = do
  cave <- P.many1 P.letter
  case cave of
    "start" -> return CaveStart
    "end"   -> return CaveEnd
    _       -> return $ Cave cave

parseConnection :: Parser Connection
parseConnection = do
  c1 <- parseCave
  P.char '-'
  c2 <- parseCave
  return $ Connection (c1,c2)

parseConnections :: Parser [Connection]
parseConnections = P.endBy parseConnection (P.spaces <|> P.eof)

-- add bidirectional connection
addConnection :: CaveMap -> Connection -> CaveMap
addConnection cm (Connection (c1,c2)) = add' c1 c2 $ add' c2 c1 cm
  where add' k v m  = M.alter (<> Just [v]) k m

parseInput :: String -> Parsed
parseInput input = do
  let Right parsed = P.parse parseConnections "(input)" input
  foldl addConnection mempty parsed

isLarge :: Cave -> Bool
isLarge (Cave cave) = isUpper $ head cave
isLarge _           = False

type MayVisitFunc = Path -> Cave -> Bool

-- keep track of the path we're busy exploring and the paths we've found
findPaths' :: MayVisitFunc -> CaveMap -> Path -> [Path] -> Cave -> [Path]
findPaths' mayvisit cm path paths start
  | start == CaveEnd  = path' : paths
  | otherwise         = foldl (findPaths' mayvisit cm path') paths validnexts
  where path'         = path <> [start]
        Just nexts    = M.lookup start cm
        validnexts    = filter (mayvisit path') nexts

findPaths :: MayVisitFunc -> CaveMap -> [Path]
findPaths f cm = findPaths' f cm mempty mempty CaveStart

-- Part 1 --
mayVisitA :: MayVisitFunc
mayVisitA path cave = isLarge cave || cave `notElem` path

part1 :: Parsed -> IO ()
part1 input = print $ length $ findPaths mayVisitA input

-- Part 2 --

mayVisitB :: MayVisitFunc
mayVisitB _ CaveStart = False
mayVisitB path cave
  | isLarge cave        = True
  | myVisits == 0       = True
  | myVisits >= 2       = False
  | nodoubles           = True
  | otherwise           = False
  where smalls    = filter (not . isLarge) path
        myVisits  = length $ filter (==cave) path
        nodoubles = length smalls == length (nub smalls)

part2 :: Parsed -> IO ()
part2 input = print $ length $ findPaths mayVisitB input

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
