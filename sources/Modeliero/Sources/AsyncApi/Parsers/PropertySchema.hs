{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.PropertySchema where

import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    type_ :: ValueType
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext = parseSchema
  where
    parseSchema schema = do
      type_ <-
        asum
          [ schema._schemaOneOf
              & fmap parseSchemaOneOf
              & (fmap . first) (Json.tagged "one-of")
          ]
          & fromMaybe (Left "no-definition")
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
            & first toJSON
            & first (Json.tagged "variant")
        error "TODO"
      error "TODO"
