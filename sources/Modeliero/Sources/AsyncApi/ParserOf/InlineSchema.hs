-- |
-- Parser of schema in the inline position.
-- That is nested inside of top-level named schema definitions in the Components section.
--
-- The difference between them is that we have different requirements for them, because we extract different information from them.
--
-- Inline schemas are used to construct anonymous types for type-signatures.
-- That's your Ints and Texts, Lists, Maybes and Tuples.
-- Outline schemas are translated into definitions of ADTs and newtypes.
module Modeliero.Sources.AsyncApi.ParserOf.InlineSchema where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.ReferencedSchema qualified as ReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    -- | Type signature.
    valueType :: ValueType,
    -- | Declarations of all nested types.
    typeDeclarations :: [(Slug, Input.Schema)]
  }

type Error = Json

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  case input._schemaType of
    Nothing ->
      Right
        Output
          { docs = "",
            valueType =
              schemaContext.contextReference
                & LocalPlainType
                & PlainValueType,
            typeDeclarations =
              [ (schemaContext.contextReference, input)
              ]
          }
    Just schemaType -> case schemaType of
      Input.OpenApiObject ->
        Right
          Output
            { docs = "",
              valueType =
                schemaContext.contextReference
                  & LocalPlainType
                  & PlainValueType,
              typeDeclarations =
                [ (schemaContext.contextReference, input)
                ]
            }
      Input.OpenApiString ->
        case input._schemaFormat of
          Just format -> case format of
            "decimal" -> fromStandardType ScientificStandardType
            "email" -> fromStandardType EmailStandardType
            "date" -> fromStandardType Iso8601DateStandardType
            "date-time" -> fromStandardType Iso8601DateTimeStandardType
            "uuid" -> fromStandardType UuidStandardType
            "ipv4" -> fromStandardType IpV4StandardType
            "ipv6" -> fromStandardType IpV6StandardType
            "hostname" -> fromStandardType HostnameStandardType
            "uri" -> fromStandardType IriStandardType
            "iri" -> fromStandardType IriStandardType
            _ ->
              -- TODO: go thru the official list: https://json-schema.org/understanding-json-schema/reference/string#built-in-formats
              Left "Unsupported format"
                & assocWithInput format
          Nothing -> do
            for_ input._schemaMinLength \_ ->
              Left "\"minLength\" is not supported in inline schemas. Only top-level type definitions can have it"
            for_ input._schemaMaxLength \_ ->
              Left "\"maxLength\" is not supported in inline schemas. Only top-level type definitions can have it"
            for_ input._schemaPattern \_ ->
              Left "\"pattern\" is not supported in inline schemas. Only top-level type definitions can have it"
            TextStandardType
              & StandardPlainType
              & fromPlainType
      Input.OpenApiNumber ->
        ScientificStandardType
          & StandardPlainType
          & fromPlainType
      Input.OpenApiInteger ->
        IntStandardType
          & StandardPlainType
          & fromPlainType
      Input.OpenApiBoolean ->
        BoolStandardType
          & StandardPlainType
          & fromPlainType
      Input.OpenApiArray ->
        case input._schemaItems of
          Nothing -> Left "Missing \"items\""
          Just items -> case items of
            Input.OpenApiItemsObject itemReferencedSchema -> do
              reference <- ReferencedSchema.parse schemaContext itemReferencedSchema
              (itemValueType, typeDeclarations) <- case reference of
                ReferencedSchema.InlineOutput itemSchema ->
                  appendContextReference "item" parse schemaContext itemSchema
                    & fmap
                      ( \output ->
                          ( output.valueType,
                            output.typeDeclarations
                          )
                      )
                ReferencedSchema.ReferenceOutput _ref slug ->
                  pure (PlainValueType (LocalPlainType slug), [])
              Right
                Output
                  { docs = input._schemaDescription & fromMaybe "",
                    valueType = VectorValueType itemValueType,
                    typeDeclarations
                  }
            Input.OpenApiItemsArray _ ->
              Left "Tuple arrays are not supported"
      Input.OpenApiNull ->
        error "TODO"
  where
    fromValueType valueType =
      Right
        Output
          { docs = input._schemaDescription & fromMaybe "",
            valueType,
            typeDeclarations = []
          }

    fromPlainType = fromValueType . PlainValueType

    fromStandardType = fromPlainType . StandardPlainType
