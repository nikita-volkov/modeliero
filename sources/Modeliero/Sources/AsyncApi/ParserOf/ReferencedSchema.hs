module Modeliero.Sources.AsyncApi.ParserOf.ReferencedSchema where

import Cases qualified
import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

data Output
  = ReferenceOutput
      -- | Name.
      Text
      Slug
  | InlineOutput Input.Schema

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext = \case
  Input.Inline schema ->
    Right (InlineOutput schema)
  Input.Ref ref -> case HashMap.lookup ref.getReference schemaContext.dict of
    Nothing ->
      Left (Json.tagged "not-found" (toJSON ref.getReference))
    Just _schema -> do
      slug <-
        ref.getReference
          & Cases.spinalize
          & specialize
          & first (Json.tagged "slug" . toJSON)
      pure (ReferenceOutput ref.getReference slug)
