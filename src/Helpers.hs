module Helpers
  ( parseInt
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

parseInt :: Parser Int
parseInt = do
  num <- P.many1 P.digit
  return $ read num
