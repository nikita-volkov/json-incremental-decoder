module Main.Decoders
(
  date,
  solrSelectResponseDocumentIDs,
  solrSelectResponseNumFound,
  module JSONIncrementalDecoder,
)
where

import Rebase.Prelude
import JSONIncrementalDecoder


date :: Value (Int, Int, Int)
date =
  objectDate `mappend` arrayDate

objectDate :: Value (Int, Int, Int)
objectDate =
  objectLookup $
  (,,) <$>
  atKey "year" numberAsInt <*>
  atKey "month" numberAsInt <*>
  atKey "day" numberAsInt

arrayDate :: Value (Int, Int, Int)
arrayDate =
  arrayElements $
  (,,) <$>
  element numberAsInt <*>
  element numberAsInt <*>
  element numberAsInt

solrSelectResponseDocumentIDs :: Value [Text]
solrSelectResponseDocumentIDs =
  objectLookup $
  atKey "response" $
  objectLookup $
  atKey "docs" $
  arrayElements $
  many $
  element $
  objectLookup $
  atKey "id" $
  string

solrSelectResponseNumFound :: Value Int
solrSelectResponseNumFound =
  objectLookup $
  atKey "response" $
  objectLookup $
  atKey "numFound" $
  numberAsInt
