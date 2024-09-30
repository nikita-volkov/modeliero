{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.ParserOf.OutlineSchema where

import Cases qualified
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.ParserOf.EnumVariants qualified as EnumVariants
import Modeliero.Sources.AsyncApi.ParserOf.InlineSchema qualified as InlineSchemaParser
import Modeliero.Sources.AsyncApi.ParserOf.OneOf qualified as OneOfParser
import Modeliero.Sources.AsyncApi.ParserOf.OneOfItemSchema qualified as OneOfItemSchemaParser
import Modeliero.Sources.AsyncApi.ParserOf.ReferencedSchema qualified as ReferencedSchemaParser
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    definition :: TypeDefinition,
    -- | Type declarations for the possible nested types.
    typeDeclarations :: [TypeDeclaration]
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  directDefinitionIfPossible <-
    label "outline" case input._schemaType of
      Nothing ->
        asum
          [ input._schemaOneOf <&> \oneOf ->
              do
                OneOfParser.Output variants unparsedTypeDeclarations <-
                  oneOf
                    & nest "one-of" (jsonifyErrors OneOfParser.parse) schemaContext
                pure (SumTypeDefinition variants, unparsedTypeDeclarations),
            input._schemaEnum
              <&> fmap (,[])
              . fmap EnumTypeDefinition
              . EnumVariants.parse schemaContext
          ]
          & sequence
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
                      nest "value" ReferencedSchemaParser.parse schemaContext valueReferencedSchema
                    case referencedSchemaOutput of
                      ReferencedSchemaParser.ReferenceOutput _ref slug ->
                        pure
                          ( Field
                              { name,
                                jsonName = nameInput,
                                docs = "",
                                type_ =
                                  packPlainType nameInput
                                    $ LocalPlainType
                                    $ slug
                              },
                            []
                          )
                      ReferencedSchemaParser.InlineOutput inlineSchema -> do
                        propertySchema <-
                          appendContextReference
                            name
                            (nest "inline-value" InlineSchemaParser.parse)
                            schemaContext
                            inlineSchema
                        pure
                          ( Field
                              { name,
                                jsonName = nameInput,
                                docs = propertySchema.docs,
                                type_ =
                                  propertySchema.valueType
                                    & packValueType nameInput
                              },
                            propertySchema.typeDeclarations
                          )
                )
              & label "properties"
              & fmap unzip
              & fmap
                ( \(fields, typeDeclarations) ->
                    ( ProductTypeDefinition schemaContext.anonymizable fields,
                      concat typeDeclarations
                    )
                )
              & fmap Just
            where
              packPlainType fieldName =
                packValueType fieldName . PlainValueType
              packValueType fieldName =
                if elem fieldName input._schemaRequired
                  then id
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
                  & Left
                  & ( \wrappedType ->
                        NewtypeDefinition
                          { wrappedType,
                            anonymizable = schemaContext.anonymizable
                          }
                    )
                  & NewtypeTypeDefinition
                  & (,[])
                  & Just
                  & pure
              _ -> pure Nothing
          _ -> pure Nothing

  (definition, unparsedTypeDeclarations) <-
    case directDefinitionIfPossible of
      Just (definition, unparsedTypeDeclarations) -> pure (definition, unparsedTypeDeclarations)
      Nothing ->
        InlineSchemaParser.parse schemaContext input
          & label "inline"
          & fmap
            ( \parsedInlineSchema ->
                ( NewtypeTypeDefinition
                    NewtypeDefinition
                      { wrappedType = Right parsedInlineSchema.valueType,
                        anonymizable = schemaContext.anonymizable
                      },
                  parsedInlineSchema.typeDeclarations
                )
            )
  typeDeclarations <-
    unparsedTypeDeclarations
      & traverse
        ( \(slug, schema) ->
            appendContextReference slug parse schemaContext schema
              & label (printCompactAs slug)
              & fmap
                ( \parsedOutlineSchema ->
                    TypeDeclaration
                      { definition = parsedOutlineSchema.definition,
                        docs = parsedOutlineSchema.docs,
                        name = slug
                      }
                      : parsedOutlineSchema.typeDeclarations
                )
        )
      & fmap concat
  pure
    Output
      { docs =
          input._schemaDescription
            & fromMaybe mempty,
        definition,
        typeDeclarations
      }
