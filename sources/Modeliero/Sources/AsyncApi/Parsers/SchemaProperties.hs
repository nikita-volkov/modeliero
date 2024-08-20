module Modeliero.Sources.AsyncApi.Parsers.SchemaProperties where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
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
