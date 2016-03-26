module JSONIncrementalDecoder.SupplementedParsers
where

import JSONIncrementalDecoder.Prelude
import JSONIncrementalDecoder.Parsers
import Data.Attoparsec.ByteString.Char8


object :: Parser a -> Supplemented Parser a
object body =
  essenceAndSupplement essence supplement
  where
    essence =
      char '{' *> skipSpace *> body
    supplement =
      skipSpace <* char '}'

row :: (a -> b -> c) -> Supplemented Parser a -> Supplemented Parser b -> Supplemented Parser c
row fn field value =
  fn <$> field <* essence (skipSpace *> char ':' *> skipSpace) <*> value

comma :: Supplemented Parser ()
comma =
  supplement $
  skipSpace *> char ',' *> skipSpace
