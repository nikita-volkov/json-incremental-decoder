module Main.Decoders
(
  date,
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
