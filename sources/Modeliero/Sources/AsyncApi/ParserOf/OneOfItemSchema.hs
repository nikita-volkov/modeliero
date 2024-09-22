module Modeliero.Sources.AsyncApi.ParserOf.OneOfItemSchema where

import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.ParserOf.InlineSchema qualified as InlineSchemaParser
import Modeliero.Sources.AsyncApi.ParserOf.ReferencedSchema qualified as ParserOf.ReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

type Output = Variant

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = assocWithInput input do
  topReferenceOutput <-
    ParserOf.ReferencedSchema.parse schemaContext input
      & first (Json.tagged "top-reference")

  variantSchema <- case topReferenceOutput of
    ParserOf.ReferencedSchema.ReferenceOutput _ _ ->
      Left "Reference where tagging object is expected"
    ParserOf.ReferencedSchema.InlineOutput schema ->
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
    nest "value" ParserOf.ReferencedSchema.parse schemaContext valueSchema >>= \case
      ParserOf.ReferencedSchema.ReferenceOutput _ref slug ->
        pure (PlainValueType (LocalPlainType slug))
      ParserOf.ReferencedSchema.InlineOutput schema ->
        nest "inline-value" InlineSchemaParser.parse schemaContext schema
          & fmap (.valueType)

  pure
    Variant
      { name = tagSlug,
        jsonName = tag,
        type_ = value,
        docs = variantSchema._schemaDescription & fromMaybe mempty,
        anonymizable = schemaContext.anonymizable
      }
