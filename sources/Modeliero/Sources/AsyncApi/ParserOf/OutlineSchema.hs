{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.ParserOf.OutlineSchema where

import Cases qualified
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.ParserOf.InlineSchema qualified as InlineSchemaParser
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as SchemaReferencedSchemaParser
import Modeliero.Sources.AsyncApi.Parsers.SumOneOf qualified as Parsers.SumOneOf
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    definition :: TypeDefinition
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  directDefinitionIfPossible <-
    label "outline" case input._schemaType of
      Nothing ->
        for input._schemaOneOf \oneOf ->
          do
            variants <-
              oneOf
                & nest "one-of" (reportifyErrors Parsers.SumOneOf.parse) schemaContext
            pure (SumTypeDefinition variants)
      Just schemaType ->
        case schemaType of
          Input.OpenApiObject -> do
            input._schemaProperties
              & InsOrd.toList
              & traverse
                ( \(nameInput, valueReferencedSchema) -> first (Json.tagged nameInput) do
                    name <-
                      nameInput
                        & Cases.spinalize
                        & specialize
                        & first toJSON
                        & assocWithInput nameInput
                        & label "name-slug"
                    referencedSchemaOutput <-
                      nest "value" SchemaReferencedSchemaParser.parse schemaContext valueReferencedSchema
                    case referencedSchemaOutput of
                      SchemaReferencedSchemaParser.ReferenceOutput _ref slug ->
                        pure
                          Field
                            { name,
                              jsonName = nameInput,
                              docs = "",
                              type_ =
                                packPlainType nameInput
                                  $ LocalPlainType
                                  $ slug
                            }
                      SchemaReferencedSchemaParser.InlineOutput inlineSchema -> do
                        propertySchema <-
                          nest "inline-value" InlineSchemaParser.parse schemaContext inlineSchema
                        pure
                          Field
                            { name,
                              jsonName = nameInput,
                              docs = propertySchema.docs,
                              type_ =
                                propertySchema.plainType
                                  & packPlainType nameInput
                            }
                )
              & label "properties"
              & fmap ProductTypeDefinition
              & fmap Just
            where
              packPlainType fieldName =
                if elem fieldName input._schemaRequired
                  then PlainValueType
                  else MaybeValueType
          Input.OpenApiString ->
            case input._schemaFormat of
              Nothing ->
                TextRestrictions
                  { regexp = Nothing,
                    minLength =
                      input._schemaMinLength
                        & fmap fromIntegral
                        & fromMaybe 0,
                    maxLength =
                      input._schemaMaxLength
                        & fmap fromIntegral
                        & fromMaybe schemaContext.config.defaultTextMaxLength,
                    charsetRangeList = Nothing
                  }
                  & TextRefinement
                  & RefinedTypeDefinition
                  & pure
                  & fmap Just
              _ -> pure Nothing
  definition <-
    label "inline" case directDefinitionIfPossible of
      Just definition -> pure definition
      Nothing ->
        InlineSchemaParser.parse schemaContext input
          & fmap (.plainType)
          & fmap PlainValueType
          & fmap ValueTypeDefinition
  pure
    Output
      { docs =
          input._schemaDescription
            & fromMaybe mempty,
        definition
      }
