module JSONIncrementalDecoder.SupplementedParsers
where

import JSONIncrementalDecoder.Prelude
import JSONIncrementalDecoder.Parsers as Parsers
import Data.Attoparsec.ByteString.Char8


{-# INLINABLE object #-}
object :: Supplemented Parser a -> Supplemented Parser a
object body =
  {-# SCC "object" #-} 
  essence openingParser *> body <* supplement closingParser
  where
    openingParser = 
      char '{' *> skipSpace
    closingParser =
      skipSpace <* char '}'

{-# INLINABLE array #-}
array :: Supplemented Parser a -> Supplemented Parser a
array body =
  {-# SCC "array" #-} 
  essence (char '[' *> skipSpace) *> body <* supplement (skipSpace <* char ']')

{-# INLINABLE row #-}
row :: (a -> b -> c) -> Supplemented Parser a -> Supplemented Parser b -> Supplemented Parser c
row fn field value =
  {-# SCC "row" #-} 
  fn <$> (field <* supplement Parsers.colon) <*> value

{-# INLINE comma #-}
comma :: Supplemented Parser ()
comma =
  {-# SCC "comma" #-} 
  supplement Parsers.comma

{-# INLINE null #-}
null :: Supplemented Parser ()
null =
  {-# SCC "null" #-} 
  essence Parsers.null

{-# INLINE stringLit #-}
stringLit :: Supplemented Parser Text
stringLit =
  {-# SCC "stringLit" #-} 
  essence Parsers.stringLitAsText

{-# INLINE anyValue #-}
anyValue :: Supplemented Parser ()
anyValue =
  {-# SCC "anyValue" #-} 
  supplement skipJSONLit

{-# INLINE anyRow #-}
anyRow :: Supplemented Parser ()
anyRow =
  {-# SCC "anyRow" #-} 
  supplement skipObjectRow

{-# INLINE anyRows #-}
anyRows :: Supplemented Parser ()
anyRows =
  {-# SCC "anyRows" #-} 
  supplement $
  skipSepBy Parsers.skipObjectRow Parsers.comma
