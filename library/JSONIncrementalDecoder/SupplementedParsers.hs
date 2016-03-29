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

{-# INLINE null #-}
null :: Supplemented Parser ()
null =
  essence Parsers.null

{-# INLINE stringLit #-}
stringLit :: Supplemented Parser Text
stringLit =
  essence Parsers.stringLitAsText

{-# INLINE anyValue #-}
anyValue :: Supplemented Parser ()
anyValue =
  supplement skipJSONLit

{-# INLINE anyRow #-}
anyRow :: Supplemented Parser ()
anyRow =
  supplement skipObjectRow

{-# INLINE anyRows #-}
anyRows :: Supplemented Parser ()
anyRows =
  supplement $
  skipSepBy Parsers.skipObjectRow Parsers.comma
