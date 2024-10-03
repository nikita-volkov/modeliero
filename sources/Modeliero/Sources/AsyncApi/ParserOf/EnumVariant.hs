module Modeliero.Sources.AsyncApi.ParserOf.EnumVariant where

import Cases qualified
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Json

type Output = EnumVariant

parse :: SchemaContext -> Input -> Either Json Output
parse _ input =
  assocWithInput input do
    first (Json.tagged "slug") case input of
      StringJson jsonName -> do
        slug <-
          jsonName
            & Cases.spinalize
            & specialize
            & first StringJson
        pure EnumVariant {slug, jsonName}
      _ ->
        Left "Not a string value type"
