module Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema where

import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.ParserOf.InlineSchema qualified as InlineSchemaParser
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

type Output = Variant

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = assocWithInput input do
  topReferenceOutput <-
    Parsers.SchemaReferencedSchema.parse schemaContext input
      & first (Json.tagged "top-reference")

  variantSchema <- case topReferenceOutput of
    Parsers.SchemaReferencedSchema.ReferenceOutput _ _ ->
      Left "Reference where tagging object is expected"
    Parsers.SchemaReferencedSchema.InlineOutput schema ->
      Right schema

  (tag, valueSchema) <-
    variantSchema._schemaProperties
      & toList
      & \case
        [] -> Left "No properties"
        [pair] -> Right pair
        properties ->
          [ ("error", "Too many properties"),
            ("input", toJSON properties)
          ]
            & Json.assocList
            & Left

  case variantSchema._schemaRequired of
    [] -> Left "Sum-tag field not listed in the required section"
    [name] ->
      if name == tag
        then pure True
        else
          Left "Unknown field property listed in the required field"
            & assocWithInput name
    required ->
      Left "Too many properties listed in the required field"
        & assocWithInput required

  tagSlug <-
    specialize tag
      & first (branchError "tag-slug" (toJSON tag) . toJSON)

  value <- do
    nest "value" Parsers.SchemaReferencedSchema.parse schemaContext valueSchema >>= \case
      Parsers.SchemaReferencedSchema.ReferenceOutput _ref slug ->
        pure (LocalPlainType slug)
      Parsers.SchemaReferencedSchema.InlineOutput schema ->
        nest "inline-value" InlineSchemaParser.parse schemaContext schema
          & fmap (.plainType)

  pure
    Variant
      { name = tagSlug,
        jsonName = tag,
        type_ = PlainValueType value,
        docs = variantSchema._schemaDescription & fromMaybe mempty,
        anonymizable = schemaContext.anonymizable
      }
