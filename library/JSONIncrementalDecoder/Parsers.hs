module JSONIncrementalDecoder.Parsers
where

import JSONIncrementalDecoder.Prelude hiding (scanl, isDigit, bool, null, takeWhile)
import Data.Attoparsec.ByteString.Char8
import qualified Data.HashMap.Strict
import qualified Control.Monad.Par
import qualified JSONIncrementalDecoder.Parsers.Aeson as Aeson


-- * General Parser
-------------------------

-- |
-- Composes two parsers to consume the same input.
-- Each must consume it in whole.
{-# INLINE parallelly #-}
parallelly :: Parser a -> Parser b -> Parser (a, b)
parallelly parser1 parser2 =
  do
    (input2, result1) <- match parser1
    result2 <- liftSubparser input2 (parser2 <* endOfInput)
    return (result1, result2)

{-# INLINABLE sequenceParallellyToList #-}
sequenceParallellyToList :: [Parser a] -> Parser [a]
sequenceParallellyToList =
  \case
    head : tail -> fmap (uncurry (:)) (sequenceParallelly head tail)
    _ -> return []

{-# INLINABLE sequenceParallelly #-}
sequenceParallelly :: Traversable t => Parser a -> t (Parser a) -> Parser (a, (t a))
sequenceParallelly primaryParser secondaryParsers =
  do
    (input, primaryResult) <- match primaryParser
    secondaryResults <- liftSubparsers input (fmap (<* endOfInput) secondaryParsers)
    return (primaryResult, secondaryResults)

{-# INLINE liftSubparsers #-}
liftSubparsers :: Traversable t => ByteString -> t (Parser a) -> Parser (t a)
liftSubparsers input parsers =
  traverse liftEither $
  parMap parserToEither parsers
  where
    parserToEither parser =
      parseOnly parser input
    parMap f xs =
      Control.Monad.Par.runPar $
      traverse (Control.Monad.Par.spawn_ . return . f) xs >>=
      traverse Control.Monad.Par.get

{-# INLINE liftSubparser #-}
liftSubparser :: ByteString -> Parser a -> Parser a
liftSubparser input parser =
  liftEither (parseOnly (parser <* endOfInput) input)

{-# INLINE liftEither #-}
liftEither :: Either String a -> Parser a
liftEither =
  either fail return


-- * Specific
-------------------------

null :: Parser ()
null =
  stringCI "null" $> ()

bool :: Parser Bool
bool =
  stringCI "false" $> False <|>
  stringCI "true" $> True

stringLitAsText :: Parser Text
stringLitAsText =
  Aeson.jstring

intLit :: Integral a => Parser a
intLit =
  signed decimal <* shouldFail (char '.')

-- |
-- An optimized parser, which skips the next valid JSON literal.
skipJSONLit :: Parser ()
skipJSONLit =
  skipStringLit <|>
  skipNumberLit <|>
  void bool <|>
  null <|>
  skipArrayLit <|>
  skipObjectLit

skipStringLit :: Parser ()
skipStringLit =
  char '"' *> contents *> char '"' $> ()
  where
    contents =
      skipWhile (\c -> c /= '"' && c /= '\\') *> ((escapeSeq *> contents) <|> pure ())
      where
        escapeSeq =
          char '\\' *> satisfy (\c -> c == '"' || c == '\\')

skipNumberLit :: Parser ()
skipNumberLit =
  sign *> oneOrMoreDigits *> pointAndAfter
  where
    oneOrMoreDigits =
      satisfy isDigit *> skipWhile isDigit
    sign =
      (satisfy (\c -> c == '-' || c == '+') $> ()) <|> pure ()
    pointAndAfter =
      (char '.' *> oneOrMoreDigits) <|> pure ()

skipObjectRow :: Parser ()
skipObjectRow =
  skipStringLit *> skipSpace *> char ':' *> skipSpace *> skipJSONLit

skipObjectLit :: Parser ()
skipObjectLit =
  objectBody (skipSepBy skipObjectRow comma)

skipArrayLit :: Parser ()
skipArrayLit =
  arrayBody (skipSepBy skipJSONLit comma)

objectBody :: Parser a -> Parser a
objectBody body =
  char '{' *> skipSpace *> body <* skipSpace <* char '}'

arrayBody :: Parser a -> Parser a
arrayBody body =
  char '[' *> skipSpace *> body <* skipSpace <* char ']'

colon :: Parser ()
colon =
  skipSpace *> char ':' *> skipSpace

comma :: Parser ()
comma =
  skipSpace *> char ',' *> skipSpace

objectKey :: Parser Text
objectKey =
  stringLitAsText <* skipSpace <* char ':' <* skipSpace
