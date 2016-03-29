-- |
-- A DSL for specification of a single-pass incremental and possibly partial parser of JSON.
-- 
module JSONIncrementalDecoder
(
  -- * Execution
  valueToSupplementedParser,
  valueToParser,
  valueToByteStringToEither,
  valueToLazyByteStringToEither,
  -- * Value
  Value,
  null,
  nullable,
  bool,
  numberAsInt,
  numberAsInteger,
  numberAsDouble,
  numberAsScientific,
  string,
  objectRows,
  objectLookup,
  arrayElements,
  -- * ObjectRows
  ObjectRows,
  row,
  anyRow,
  -- * ObjectLookup
  ObjectLookup,
  atKey,
  -- * ArrayElements
  ArrayElements,
  element,
  anyElement,
  -- * Matcher
  Matcher,
  equals,
  satisfies,
  converts,
  whatever,
)
where

import JSONIncrementalDecoder.Prelude hiding (String, null, bool)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy
import qualified JSONIncrementalDecoder.SupplementedParsers as SupplementedParsers
import qualified JSONIncrementalDecoder.Parsers as Parsers
import qualified Matcher


-- |
-- Converts the Value specification into a Supplemented Attoparsec Parser.
valueToSupplementedParser :: Value a -> Supplemented Parser a
valueToSupplementedParser (Value impl) =
  impl

-- |
-- Essentially just a helper, which is the same as
-- 
-- @
-- 'runSupplemented' . 'valueToSupplementedParser'
-- @
valueToParser :: Value a -> Parser (a, Parser ())
valueToParser =
  runSupplemented .
  valueToSupplementedParser

-- |
-- Converts the Value specification into a function,
-- which decodes a strict ByteString.
valueToByteStringToEither :: Value a -> ByteString -> Either Text a
valueToByteStringToEither value input =
  either (Left . fromString) Right $
  Data.Attoparsec.ByteString.Char8.parseOnly parser input
  where
    parser =
      fmap fst $
      valueToParser value

-- |
-- Converts the Value specification into a function,
-- which decodes a strict LazyByteString.
valueToLazyByteStringToEither :: Value a -> Data.ByteString.Lazy.ByteString -> Either Text a
valueToLazyByteStringToEither value input =
  either (Left . fromString) Right $
  Data.Attoparsec.ByteString.Lazy.eitherResult $
  Data.Attoparsec.ByteString.Lazy.parse parser input
  where
    parser =
      fmap fst $
      valueToParser value


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

{-# INLINE nullable #-}
nullable :: Value a -> Value (Maybe a)
nullable (Value p) =
  Value (mplus (fmap Just p) (fmap (const Nothing) SupplementedParsers.null))

{-# INLINE bool #-}
bool :: Value Bool
bool =
  Value (lift Parsers.bool)

{-# INLINE numberAsInt #-}
numberAsInt :: Value Int
numberAsInt =
  Value (lift Parsers.numberLitAsIntegral)

{-# INLINE numberAsInteger #-}
numberAsInteger :: Value Integer
numberAsInteger =
  Value (lift Parsers.numberLitAsIntegral)

{-# INLINE numberAsDouble #-}
numberAsDouble :: Value Double
numberAsDouble =
  Value (lift Parsers.numberLitAsDouble)

{-# INLINE numberAsScientific #-}
numberAsScientific :: Value Scientific
numberAsScientific =
  Value (lift Parsers.numberLitAsScientific)

{-# INLINE string #-}
string :: Value Text
string =
  Value $
  SupplementedParsers.stringLit

{-# INLINABLE stringMatcher #-}
stringMatcher :: Matcher Text a -> Value a
stringMatcher matcher =
  Value $
  SupplementedParsers.stringLit >>=
  either (const mzero) return . Matcher.run matcher

{-# INLINABLE objectRows #-}
objectRows :: ObjectRows a -> Value a
objectRows (ObjectRows interspersedSupplementedParser) =
  Value (SupplementedParsers.object supplementedParser)
  where
    supplementedParser =
      runInterspersed interspersedSupplementedParser SupplementedParsers.comma

{-# INLINABLE objectLookup #-}
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

{-# INLINABLE arrayElements #-}
arrayElements :: ArrayElements a -> Value a
arrayElements (ArrayElements interspersedSupplementedParser) =
  Value (SupplementedParsers.array supplementedParser)
  where
    supplementedParser =
      runInterspersed interspersedSupplementedParser SupplementedParsers.comma

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


-- * ArrayElements
-------------------------

newtype ArrayElements a =
  ArrayElements (Interspersed (Supplemented Parser) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE element #-}
element :: Value a -> ArrayElements a
element (Value value) =
  ArrayElements (lift value)

{-# INLINE anyElement #-}
anyElement :: ArrayElements ()
anyElement =
  ArrayElements (lift (SupplementedParsers.anyValue))
