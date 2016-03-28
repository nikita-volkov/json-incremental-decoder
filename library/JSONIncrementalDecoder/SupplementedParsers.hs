module JSONIncrementalDecoder.SupplementedParsers
where

import JSONIncrementalDecoder.Prelude
import JSONIncrementalDecoder.Parsers
import Data.Attoparsec.ByteString.Char8


object :: Supplemented Parser a -> Supplemented Parser a
object body =
  essence openingParser *> body <* supplement closingParser
  where
    openingParser = 
      char '{' *> skipSpace
    closingParser =
      skipSpace <* char '}'

row :: (a -> b -> c) -> Supplemented Parser a -> Supplemented Parser b -> Supplemented Parser c
row fn field value =
  fn <$> field <*> (essence (skipSpace *> char ':' *> skipSpace) *> value)

comma :: Supplemented Parser ()
comma =
  supplement $
  skipSpace *> char ',' *> skipSpace

stringLit :: Supplemented Parser Text
stringLit =
  undefined

skipValue :: Supplemented Parser ()
skipValue =
  supplement $
  skipJSONLit
