module Main where

import Rebase.Prelude hiding (takeWhile)
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified JSONIncrementalDecoder.DSL as DSL
import qualified Main.Decoders as Decoders


main =
  defaultMain $
  testGroup "All tests"
  [
    testCase "Primitive" $
    let
      input =
        "{\"year\" : \"2001\", \"month\": \"1\", \"day\": \"2\"}"
      result =
        Decoders.value_byteStringToEither Decoders.dateStrings input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "In different order" $
    let
      input =
        "{\"month\": \"1\", \"day\": \"2\", \"year\" : \"2001\"}"
      result =
        Decoders.value_byteStringToEither Decoders.dateStrings input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "With redundant fields" $
    let
      input =
        "{\"redundant1\": \"4\", \"month\": \"1\", \"redundant2\": \"3\", \"day\": \"2\", \"year\" : \"2001\"}"
      result =
        Decoders.value_byteStringToEither Decoders.dateStrings input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "With trailing fields" $
    let
      input =
        "{\"month\": \"1\", \"day\": \"2\", \"year\" : \"2001\", \"trailing\": \"3\", \"trailing\": \"4\"}"
      result =
        Decoders.value_byteStringToEither Decoders.dateStrings input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
  ]
