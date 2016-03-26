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


newtype Object a =
  Object (Interspersed (Supplemented Parser) a)
  deriving (Functor, Applicative, Alternative)

{-# INLINE row #-}
row :: (a -> b -> c) -> String a -> Value b -> Object c
row combine (String string) (Value value) =
  Object (lift (SupplementedParsers.row combine string value))



newtype Lookup key context result =
  Lookup (ReaderT (forall a. key -> Value a -> context a) (Unsequential context) result)
  deriving (Functor, Applicative)

at :: Monad context => key -> Value a -> Lookup key context a
at key value =
  Lookup $
  ReaderT $
  unsequential . foolTheCompiler key value
  where
    foolTheCompiler :: key -> Value a -> (forall a. key -> Value a -> context a) -> context a
    foolTheCompiler key value row =
      row key value
