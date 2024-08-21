module Modeliero.Sources.AsyncApi.Parsers.SumOneOf where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = [Input.Referenced Input.Schema]

type Output = [Parsers.SumVariantSchema.Output]

data Error
  = VariantError
      Int
      Parsers.SumVariantSchema.Error
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  input
    & zip (enumFrom 0)
    & traverse
      ( \(index, referencedSchemaInput) ->
          Parsers.SumVariantSchema.parse schemaContext referencedSchemaInput
            & first (VariantError index)
      )
