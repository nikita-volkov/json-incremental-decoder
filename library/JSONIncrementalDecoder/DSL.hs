module JSONIncrementalDecoder.DSL
where

import JSONIncrementalDecoder.Prelude hiding (String)
import Data.Attoparsec.ByteString.Char8
import qualified JSONIncrementalDecoder.SupplementedParsers as SupplementedParsers
import qualified JSONIncrementalDecoder.Parsers as Parsers


newtype Value a =
  Value (Supplemented Parser a)

null :: Value ()
null =
  undefined

string :: String a -> Value a
string (String string) =
  Value string

object :: Object a -> Value a
object (Object interspersedSupplementedParser) =
  Value (essence objectOpeningParser *> supplementedParser <* supplement objectClosingParser)
  where
    objectOpeningParser = 
      char '{' *> skipSpace
    objectClosingParser =
      skipSpace <* char '}'
    supplementedParser =
      runInterspersed interspersedSupplementedParser SupplementedParsers.comma


newtype String a =
  String (Supplemented Parser a)

matcher_string :: Matcher Text a -> String a
matcher_string =
  undefined


newtype Object a =
  Object (Interspersed (Supplemented Parser) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE row #-}
row :: (a -> b -> c) -> String a -> Value b -> Object c
row combine (String string) (Value value) =
  Object (lift (SupplementedParsers.row combine string value))

skipRow :: Object ()
skipRow =
  row (const (const ())) (matcher_string whatever) undefined

lookup_object :: Lookup Text Object a -> Object a
lookup_object (Lookup lookupImpl) =
  runUnsequential (runReaderT lookupImpl lookupRow) skipRow (return ()) <*
  skipMany skipRow
  where
    lookupRow =
      LookupRow $
      \key value ->
        row (const id) (matcher_string (equals key)) value


newtype Lookup key context result =
  Lookup (ReaderT (LookupRow key context) (Unsequential context) result)
  deriving (Functor, Applicative)

at :: Monad context => key -> Value a -> Lookup key context a
at key value =
  Lookup $
  ReaderT $
  \(LookupRow lookupRow) ->
    unsequential (lookupRow key value)


-- |
-- Required to work around the ImpredicativeTypes insanity.
newtype LookupRow key context =
  LookupRow (forall a. key -> Value a -> context a)
