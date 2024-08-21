{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.ParserOf.Schema where

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.Properties qualified as ParserOf.Properties
import Modeliero.Sources.AsyncApi.Parsers.SumOneOf qualified as Parsers.SumOneOf
import Modeliero.Sources.AsyncApi.Parsers.SumSchema qualified as Parsers.SumSchema
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    definition :: TypeDefinition
  }

data Error
  = NoTypeDefinitionError
  | SumOneOfError Parsers.SumOneOf.Error
  | PropertiesError ParserOf.Properties.Error
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  definition <-
    case input._schemaType of
      Just schemaType -> case schemaType of
        Input.OpenApiObject ->
          asum
            [ input._schemaProperties
                & filtered InsOrdHashMap.null Just
                & fmap
                  ( \properties ->
                      ParserOf.Properties.parse schemaContext properties
                        & first PropertiesError
                        & fmap ProductTypeDefinition
                  ),
              input._schemaOneOf <&> \oneOf ->
                do
                  sumOneOfOutput <-
                    Parsers.SumOneOf.parse schemaContext oneOf
                      & first SumOneOfError

                  variants <- for sumOneOfOutput \sumVariantSchemaOutput -> do
                    error "TODO"
                  pure (SumTypeDefinition variants)
            ]
            & fromMaybe (Left NoTypeDefinitionError)
        Input.OpenApiString ->
          case input._schemaFormat of
            Just format -> case format of
              "email" ->
                EmailStandardType
                  & StandardPlainType
                  & PlainValueType
                  & ValueTypeDefinition
                  & pure
              _ -> error "TODO"
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
  pure
    Output
      { docs =
          input._schemaDescription
            & fromMaybe mempty,
        definition
      }
