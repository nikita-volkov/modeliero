module Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

data Output = Output
  { tag :: Text,
    slug :: Slug,
    docs :: Text,
    schema :: Input.Schema
  }

data Error
  = NoPropertiesError
  | TooManyPropertiesError
      -- | Properties.
      [(Text, Input.Referenced Input.Schema)]
  | SlugParsingError Text
  | ReferenceError Parsers.SchemaReferencedSchema.Error
  | TaggedReferenceError
      -- | Tag.
      Text
      Parsers.SchemaReferencedSchema.Error
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  inputSchema <-
    Parsers.SchemaReferencedSchema.parse schemaContext input
      & first ReferenceError
  inputSchema._schemaProperties
    & toList
    & \case
      [] -> Left NoPropertiesError
      [(tag, taggedReferencedSchemaInput)] -> do
        slug <-
          specialize tag
            & first SlugParsingError
        schema <-
          Parsers.SchemaReferencedSchema.parse schemaContext taggedReferencedSchemaInput
            & first (TaggedReferenceError tag)
        pure
          Output
            { docs = inputSchema._schemaDescription & fromMaybe mempty,
              ..
            }
      properties -> Left (TooManyPropertiesError properties)
