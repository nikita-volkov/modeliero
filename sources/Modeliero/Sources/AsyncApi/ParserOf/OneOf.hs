module Modeliero.Sources.AsyncApi.ParserOf.OneOf where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.OneOfItemSchema qualified as ParserOf.OneOfItemSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = [Input.Referenced Input.Schema]

data Output = Output
  { variants :: [Variant],
    typeDeclarations :: [(Slug, Input.Schema)]
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  input
    & zip (enumFrom @Int 0)
    & traverse
      ( \(index, referencedSchemaInput) ->
          ParserOf.OneOfItemSchema.parse schemaContext referencedSchemaInput
            & label (fromString (show index))
            & fmap
              ( \x ->
                  (x.variant, x.typeDeclarations)
              )
      )
    & fmap unzip
    & fmap
      ( \(variants, typeDeclarationsList) ->
          Output
            { variants,
              typeDeclarations = typeDeclarationsList & concat
            }
      )
