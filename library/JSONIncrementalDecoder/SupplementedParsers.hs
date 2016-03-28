module JSONIncrementalDecoder.SupplementedParsers
where

import JSONIncrementalDecoder.Prelude
import JSONIncrementalDecoder.Parsers as Parsers
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
  fn <$> (field <* supplement Parsers.colon) <*> value

{-# INLINE comma #-}
comma :: Supplemented Parser ()
comma =
  supplement Parsers.comma

stringLit :: Supplemented Parser Text
stringLit =
  essence Parsers.stringLitAsText

{-# INLINE skipValue #-}
skipValue :: Supplemented Parser ()
skipValue =
  supplement skipJSONLit

{-# INLINE skipRow #-}
skipRow :: Supplemented Parser ()
skipRow =
  supplement skipObjectRow

skipRows :: Supplemented Parser ()
skipRows =
  supplement $
  skipSepBy Parsers.skipObjectRow Parsers.comma
