module JSONIncrementalDecoder.DSL
(
  value_supplementedParser,
  value_parser,
  value_byteStringToEither,
  -- * Value
  Value,
  null,
  string,
  stringMatcher,
  objectRows,
  objectLookup,
  anyValue,
  -- * ObjectRows
  ObjectRows,
  row,
  anyRow,
  -- * ObjectLookup
  ObjectLookup,
  atKey,
  -- * Matcher
  Matcher,
  equals,
  satisfies,
  converts,
  whatever,
)
where

import JSONIncrementalDecoder.Prelude hiding (String, null)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import qualified JSONIncrementalDecoder.SupplementedParsers as SupplementedParsers
import qualified JSONIncrementalDecoder.Parsers as Parsers
import qualified Matcher


value_supplementedParser :: Value a -> Supplemented Parser a
value_supplementedParser (Value impl) =
  impl

value_parser :: Value a -> Parser (a, Parser ())
value_parser =
  runSupplemented .
  value_supplementedParser

value_byteStringToEither :: Value a -> ByteString -> Either Text a
value_byteStringToEither value input =
  either (Left . fromString) Right $
  Data.Attoparsec.ByteString.Char8.parseOnly parser input
  where
    parser =
      fmap fst $
      value_parser value


-- * Value
-------------------------

newtype Value a =
  Value (Supplemented Parser a)
  deriving (Functor)

-- |
-- Provides support for alternatives.
-- 
-- E.g,
-- 
-- >fmap Left bool <> fmap Right string
-- 
-- will succeed for either a Boolean or String value.
instance Monoid (Value a) where
  {-# INLINE mempty #-}
  mempty =
    Value empty
  {-# INLINE mappend #-}
  mappend (Value a) (Value b) =
    Value (a <|> b)

{-# INLINE null #-}
null :: Value ()
null =
  Value $
  SupplementedParsers.null

{-# INLINE string #-}
string :: Value Text
string =
  Value $
  SupplementedParsers.stringLit

stringMatcher :: Matcher Text a -> Value a
stringMatcher matcher =
  Value $
  SupplementedParsers.stringLit >>=
  either (const mzero) return . Matcher.run matcher

objectRows :: ObjectRows a -> Value a
objectRows (ObjectRows interspersedSupplementedParser) =
  Value (SupplementedParsers.object supplementedParser)
  where
    supplementedParser =
      runInterspersed interspersedSupplementedParser SupplementedParsers.comma

objectLookup :: ObjectLookup a -> Value a
objectLookup (ObjectLookup lookupImpl) =
  objectRows $
  runUnsequential lookupImpl anyRow <*
  remainders
  where
    remainders =
      ObjectRows $
      lift $
      SupplementedParsers.anyRows

-- |
-- Matches any value.
{-# INLINE anyValue #-}
anyValue :: Value ()
anyValue =
  Value SupplementedParsers.anyValue


-- * ObjectRows
-------------------------

newtype ObjectRows a =
  ObjectRows (Interspersed (Supplemented Parser) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINABLE row #-}
row :: (a -> b -> c) -> Matcher Text a -> Value b -> ObjectRows c
row combine keyMatcher (Value value) =
  ObjectRows (lift (SupplementedParsers.row combine key value))
  where
    key =
      SupplementedParsers.stringLit >>=
      either (const mzero) return . Matcher.run keyMatcher

{-# INLINE anyRow #-}
anyRow :: ObjectRows ()
anyRow =
  ObjectRows (lift (SupplementedParsers.anyRow))


-- * ObjectLookup
-------------------------

newtype ObjectLookup a =
  ObjectLookup (Unsequential ObjectRows a)
  deriving (Functor, Applicative)

{-# INLINE atKey #-}
atKey :: Text -> Value a -> ObjectLookup a
atKey key value =
  ObjectLookup $
  unsequential $
  row (const id) (equals key) value

