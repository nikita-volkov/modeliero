{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.ParserOf.Properties where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Parsers.PropertySchema qualified as Parsers.PropertySchema
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = InsOrd.InsOrdHashMap Text (OpenApi.Referenced OpenApi.Schema)

type Output = [Field]

data Error
  = PropertyError
      -- | Property name.
      Text
      -- | Input.
      Parsers.PropertySchema.Input
      -- | Reason.
      Parsers.PropertySchema.Error
  | PropertyDereferencingError
      -- | Property name.
      Text
      Parsers.SchemaReferencedSchema.Error
  | PropertyKeyError
      -- | Property name
      Text
      -- | Reason.
      Text
  deriving (Eq, Show, Generic)

instance ToJSON Error where
  toJSON = \case
    PropertyError propertyName input reason ->
      [ ("name", propertyName & toJSON),
        ("input", input & toJSON),
        ("reason", reason & toJSON)
      ]
        & Json.assocList
        & Json.tagged "property"

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  input
    & InsOrd.toList
    & traverse
      ( \(nameInput, referencedSchemaInput) -> do
          name <-
            nameInput
              & specialize
              & first (PropertyKeyError nameInput)
          schema <-
            referencedSchemaInput
              & Parsers.SchemaReferencedSchema.parse schemaContext
              & first (PropertyDereferencingError nameInput)
          propertySchema <-
            schema
              & Parsers.PropertySchema.parse schemaContext
              & first (PropertyError nameInput schema)
          pure
            Field
              { name,
                jsonName = nameInput,
                docs = propertySchema.docs,
                type_ = propertySchema.type_
              }
      )
