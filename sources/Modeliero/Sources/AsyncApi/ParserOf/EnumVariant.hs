module Modeliero.Sources.AsyncApi.ParserOf.EnumVariant where

import Cases qualified
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Json

type Output = EnumVariant

parse :: SchemaContext -> Input -> Either Json Output
parse _ input = assocWithInput input do
  slug <- first (Json.tagged "slug") case input of
    StringJson stringValue ->
      stringValue
        & Cases.spinalize
        & specialize
        & first StringJson
    _ ->
      Left "Not a string value type"
  pure EnumVariant {slug, json = input}
