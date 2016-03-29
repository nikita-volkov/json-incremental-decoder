module Main.Decoders
(
  dateStrings,
  module JSONIncrementalDecoder.DSL,
)
where

import Rebase.Prelude
import JSONIncrementalDecoder.DSL


dateStrings :: Value (Text, Text, Text)
dateStrings =
  objectLookup $
  (,,) <$>
  atKey "year" string <*>
  atKey "month" string <*>
  atKey "day" string

