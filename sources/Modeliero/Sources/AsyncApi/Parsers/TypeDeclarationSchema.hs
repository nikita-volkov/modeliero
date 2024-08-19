{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.TypeDeclarationSchema where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SumOneOf qualified as Parsers.SumOneOf
import Modeliero.Sources.AsyncApi.Parsers.SumSchema qualified as Parsers.SumSchema
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    definition :: TypeDefinition
  }

data Error
  = NoTypeDefinitionError
  | SumOneOfError Parsers.SumOneOf.Error

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  asum
    [ input._schemaOneOf <&> \oneOf ->
        do
          sumOneOfOutput <-
            Parsers.SumOneOf.parse schemaContext oneOf
              & first SumOneOfError

          variants <- for sumOneOfOutput \sumVariantSchemaOutput -> do
            error "TODO"
          pure
            Output
              { docs =
                  input._schemaDescription
                    & fromMaybe mempty,
                definition = SumTypeDefinition variants
              }
    ]
    & fromMaybe (Left NoTypeDefinitionError)
