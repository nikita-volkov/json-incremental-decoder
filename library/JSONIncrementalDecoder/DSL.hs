module JSONIncrementalDecoder.DSL
(
  value_supplementedParser,
  value_parser,
  value_byteStringToEither,
  -- * Value
  Value,
  null,
  string,
  matcher_string,
  object,
  -- * Object
  Object,
  row,
  skipRow,
  lookup_object,
  -- * Lookup
  Lookup,
  at,
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
  mempty =
    Value empty
  mappend (Value a) (Value b) =
    Value (a <|> b)

null :: Value ()
null =
  undefined

string :: Value Text
string =
  Value $
  SupplementedParsers.stringLit

matcher_string :: Matcher Text a -> Value a
matcher_string matcher =
  Value $
  SupplementedParsers.stringLit >>=
  either (const mzero) return . Matcher.run matcher

object :: Object a -> Value a
object (Object interspersedSupplementedParser) =
  Value (SupplementedParsers.object supplementedParser)
  where
    supplementedParser =
      runInterspersed interspersedSupplementedParser SupplementedParsers.comma

anyValue :: Value ()
anyValue =
  Value SupplementedParsers.skipValue


-- * Object
-------------------------

newtype Object a =
  Object (Interspersed (Supplemented Parser) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE row #-}
row :: (a -> b -> c) -> Matcher Text a -> Value b -> Object c
row combine keyMatcher (Value value) =
  Object (lift (SupplementedParsers.row combine key value))
  where
    key =
      SupplementedParsers.stringLit >>=
      either (const mzero) return . Matcher.run keyMatcher

skipRow :: Object ()
skipRow =
  Object (lift (SupplementedParsers.skipRow))

lookup_object :: Lookup Text Object a -> Object a
lookup_object (Lookup lookupImpl) =
  runUnsequential (runReaderT lookupImpl lookupRow) skipRow <*
  remainders
  where
    lookupRow =
      LookupRow $
      \key value ->
        row (const id) (equals key) value
    remainders =
      Object $
      lift $
      SupplementedParsers.skipRows


-- * Lookup
-------------------------

newtype Lookup k m a =
  Lookup (ReaderT (LookupRow k m) (Unsequential m) a)
  deriving (Functor, Applicative)

-- |
-- Required to work around the ImpredicativeTypes insanity.
newtype LookupRow key context =
  LookupRow (forall a. key -> Value a -> context a)

at :: Monad m => k -> Value a -> Lookup k m a
at key value =
  Lookup $
  ReaderT $
  \(LookupRow lookupRow) ->
    unsequential (lookupRow key value)

