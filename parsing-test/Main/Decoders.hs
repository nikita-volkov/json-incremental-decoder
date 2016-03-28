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
  object $
  lookup_object $
  (,,) <$>
  at "year" string <*>
  at "month" string <*>
  at "day" string

