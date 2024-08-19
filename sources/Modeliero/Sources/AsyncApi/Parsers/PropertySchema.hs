{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.PropertySchema where

import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    type_ :: ValueType
  }

data Error
  = OneOfError
      -- | Offset.
      Int
      -- | Input.
      Input.Schema
      OneOfError
  | NoDefinitionError

data OneOfError
  = -- | The sum one-of pattern was recognized, but there is an error parsing the sum.
    SumPatternOneOfError
      -- | Sum tag name.
      Text
      -- | Reason.
      SumError

data SumError

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext = parseSchema
  where
    parseSchema schema = do
      type_ <-
        asum
          [ schema._schemaOneOf
              & fmap parseSchemaOneOf
          ]
          & fromMaybe (Left NoDefinitionError)
      pure
        Output
          { docs = error "TODO",
            type_ = type_
          }

    parseSchemaOneOf :: [Input.Referenced Input.Schema] -> Either Error ValueType
    parseSchemaOneOf oneOf = do
      for oneOf \referencedSchemaInput -> do
        variantSchemaInput <-
          Parsers.SchemaReferencedSchema.parse schemaContext referencedSchemaInput
            & first (error "TODO: Adapt error")
        error "TODO"
      error "TODO"
