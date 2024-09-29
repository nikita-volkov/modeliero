module Modeliero.Sources.AsyncApi.ParserOf.OneOfItemSchema where

import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.ParserOf.InlineSchema qualified as InlineSchemaParser
import Modeliero.Sources.AsyncApi.ParserOf.ReferencedSchema qualified as ParserOf.ReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Referenced Input.Schema

data Output = Output
  { variant :: Variant,
    typeDeclarations :: [(Slug, Input.Schema)]
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = assocWithInput input do
  topReferenceOutput <-
    ParserOf.ReferencedSchema.parse schemaContext input
      & first (Json.tagged "top-reference")

  variantSchema <- case topReferenceOutput of
    ParserOf.ReferencedSchema.ReferenceOutput ref _ ->
      case HashMap.lookup ref schemaContext.dict of
        Just schema -> Right schema
        Nothing -> Left $ Json.tagged ref "Schema not found"
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

  (type_, typeDeclarations) <- do
    nest "value" ParserOf.ReferencedSchema.parse schemaContext valueSchema >>= \case
      ParserOf.ReferencedSchema.ReferenceOutput _ref slug ->
        pure (PlainValueType (LocalPlainType slug), [])
      ParserOf.ReferencedSchema.InlineOutput schema ->
        appendContextReference tagSlug (nest "inline-value" InlineSchemaParser.parse) schemaContext schema
          & fmap (\x -> (x.valueType, x.typeDeclarations))

  pure
    Output
      { variant =
          Variant
            { name = tagSlug,
              jsonName = tag,
              type_,
              docs = variantSchema._schemaDescription & fromMaybe mempty,
              anonymizable = schemaContext.anonymizable
            },
        typeDeclarations
      }
