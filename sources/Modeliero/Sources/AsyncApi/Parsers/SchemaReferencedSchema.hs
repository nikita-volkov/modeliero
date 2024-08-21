module Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema where

import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

type Output = Input.Schema

data Error
  = NotFoundError Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext = \case
  Input.Ref ref -> case HashMap.lookup ref.getReference schemaContext.dict of
    Nothing ->
      Left (NotFoundError ref.getReference)
    Just schema ->
      Right schema
  Input.Inline schema -> pure schema
