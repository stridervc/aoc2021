module Helpers
  ( parseInt
  , sort
  , sortOn
  , transpose
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.List (sort, sortOn, transpose)

parseInt :: Parser Int
parseInt = do
  num <- P.many1 P.digit
  return $ read num

