module JSONIncrementalDecoder.DSL
(
  value_supplementedParser,
  value_parser,
  -- * Value
  Value,
  null,
  string,
  object,
  -- * Object
  Object,
  row,
  skipRow,
  lookup_object,
  -- * Lookup
  Lookup,
  at,
)
where

import JSONIncrementalDecoder.Prelude hiding (String, null)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified JSONIncrementalDecoder.SupplementedParsers as SupplementedParsers
import qualified JSONIncrementalDecoder.Parsers as Parsers
import qualified Matcher


value_supplementedParser :: Value a -> Supplemented Parser a
value_supplementedParser (Value a) =
  undefined

value_parser :: Value a -> Parser (a, Parser ())
value_parser =
  runSupplemented .
  value_supplementedParser


-- * Value
-------------------------

newtype Value a =
  Value (Supplemented Parser a)

null :: Value ()
null =
  undefined

string :: Matcher Text a -> Value a
string matcher =
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
  row (const (const ())) whatever anyValue

lookup_object :: Lookup Text Object a -> Object a
lookup_object (Lookup lookupImpl) =
  runUnsequential (runReaderT lookupImpl lookupRow) skipRow (return ()) <*
  skipMany skipRow
  where
    lookupRow =
      LookupRow $
      \key value ->
        row (const id) (equals key) value


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

