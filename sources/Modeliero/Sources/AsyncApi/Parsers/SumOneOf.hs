module Modeliero.Sources.AsyncApi.Parsers.SumOneOf where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = [Input.Referenced Input.Schema]

type Output = [Parsers.SumVariantSchema.Output]

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  input
    & zip (enumFrom 0)
    & traverse
      ( \(index, referencedSchemaInput) ->
          Parsers.SumVariantSchema.parse schemaContext referencedSchemaInput
            & label (fromString (show index))
      )
