module Day13
  ( solve
  ) where

import Helpers
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import qualified Data.Map.Strict as M

type Coord        = (Int,Int)
type Paper        = M.Map Coord Bool
data Instruction  = FoldHorisontal Int | FoldVertical Int deriving (Eq,Show)

type Parsed = (Paper,[Instruction])

parseCoord :: Parser Coord
parseCoord = do
  x <- parseInt
  P.char ','
  y <- parseInt
  return (x,y)

parseInstruction :: Parser Instruction
parseInstruction = do
  P.string "fold along "
  vh <- P.letter
  P.char '='
  value <- parseInt

  case vh of
    'x' -> return $ FoldVertical value
    _   -> return $ FoldHorisontal value

parseAll :: Parser ([Coord],[Instruction])
parseAll = do
  coords <- P.many (parseCoord <* P.newline)
  P.newline
  instructions <- P.endBy parseInstruction (P.spaces <|> P.eof)
  return (coords, instructions)

mark :: Paper -> Coord -> Paper
mark paper coord = M.insert coord True paper

parseInput :: String -> Parsed
parseInput input = do
  let Right parsed = P.parse parseAll "(input)" input
  let paper = foldl mark mempty (fst parsed)
  (paper,snd parsed)

maxX :: Paper -> Int
maxX paper = maximum $ map fst $ M.keys paper

maxY :: Paper -> Int
maxY paper = maximum $ map snd $ M.keys paper

moveMark :: Paper -> Coord -> Coord -> Paper
moveMark paper src dst
  | srcmarked = M.delete src $ mark paper dst
  | otherwise = paper
  where srcmarked = M.findWithDefault False src paper

foldVertical :: Paper -> Int -> Paper
foldVertical paper i = foldl (\p (src,dst) -> moveMark p src dst) paper srcdsts
  where sourceXs  = [i+1..maxX paper]
        destXs    = [i-1,i-2..0]
        ys        = [0..maxY paper]
        maxI      = minimum [length sourceXs, length destXs] - 1
        srcdsts   = [((sourceXs!!i,y),(destXs!!i,y)) | i <- [0..maxI], y <- ys]

foldHorisontal :: Paper -> Int -> Paper
foldHorisontal paper i = foldl (\p (src,dst) -> moveMark p src dst) paper srcdsts
  where sourceYs  = [i+1..maxY paper]
        destYs    = [i-1,i-2..0]
        xs        = [0..maxX paper]
        maxI      = minimum [length sourceYs, length destYs] - 1
        srcdsts   = [((x,sourceYs!!i),(x,destYs!!i)) | i <- [0..maxI], x <- xs]

applyInstruction :: Paper -> Instruction -> Paper
applyInstruction paper (FoldHorisontal i) = foldHorisontal paper i
applyInstruction paper (FoldVertical i)   = foldVertical paper i

-- Part 1 --

part1 :: Parsed -> IO ()
part1 (paper,instructions) = do
  let applied = applyInstruction paper $ head instructions
  print $ length $ filter id $ M.elems applied

-- Part 2 --

putPaper :: Paper -> IO ()
putPaper paper = mapM_ (putStrLn . line) [0..maxY paper]
  where line y      = map (tochar y) [0..maxX paper]
        tochar y x  = if M.findWithDefault False (x,y) paper then '#' else ' '

part2 :: Parsed -> IO ()
part2 (paper,instructions) = do
  let applied = foldl applyInstruction paper instructions
  putStrLn ""
  putPaper applied

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
