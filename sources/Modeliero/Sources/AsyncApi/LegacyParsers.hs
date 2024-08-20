{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.LegacyParsers where

import Coalmine.Prelude
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
import Modeliero.AsyncApi qualified as AsyncApi
import Modeliero.Codegens.Haskell.Params

data ExtendedSchemaError
  = SchemaTitleExtendedSchemaError
      -- | Input.
      (Maybe Text)
      -- | Reason.
      SchemaTitleError
  | NoDefinitionExtendedSchemaError

parseExtendedSchema :: AsyncApi.ExtendedSchema -> Either ExtendedSchemaError TypeDeclaration
parseExtendedSchema extendedSchema = do
  name <-
    parseSchemaTitle extendedSchema.base._schemaTitle
      & first (SchemaTitleExtendedSchemaError extendedSchema.base._schemaTitle)
  docs <-
    extendedSchema.base._schemaDescription
      & fromMaybe mempty
      & pure
  definition <-
    asum
      [ extendedSchema.base._schemaAllOf <&> \spec ->
          pure (error "TODO" :: TypeDefinition),
        extendedSchema.base._schemaOneOf <&> \spec ->
          pure (error "TODO" :: TypeDefinition),
        extendedSchema.base._schemaAnyOf <&> \spec ->
          pure (error "TODO" :: TypeDefinition)
      ]
      & fromMaybe (Left NoDefinitionExtendedSchemaError)
  pure TypeDeclaration {..}

parseSchemaProperties ::
  Bool ->
  SchemaDict ->
  InsOrd.InsOrdHashMap Text (OpenApi.Referenced OpenApi.Schema) ->
  Either Text [Field]
parseSchemaProperties =
  error "TODO"

data SchemaTitleError
  = MissingSchemaTitleError
  | PresentSchemaTitleError Text

parseSchemaTitle ::
  Maybe Text ->
  Either SchemaTitleError Slug
parseSchemaTitle = \case
  Nothing ->
    Left MissingSchemaTitleError
  Just text ->
    specialize text
      & first PresentSchemaTitleError

data AllOfError

parseAllOf ::
  [OpenApi.Referenced OpenApi.Schema] ->
  Either AllOfError TypeDefinition
parseAllOf =
  error "TODO"

data AllOfReferencedSchemaError
  = SchemaAllOfReferencedSchemaError
      ValueSchemaError

parseAllOfReferencedSchema ::
  Bool ->
  SchemaDict ->
  OpenApi.Referenced OpenApi.Schema ->
  Either AllOfReferencedSchemaError ValueType
parseAllOfReferencedSchema anonymizable schemaDict = \case
  OpenApi.Ref ref ->
    error "TODO"
  OpenApi.Inline schema -> do
    parseValueSchema
      anonymizable
      schemaDict
      schema
      & first SchemaAllOfReferencedSchemaError

data ValueSchemaError

-- | Parse schema in Value position
parseValueSchema ::
  Bool ->
  SchemaDict ->
  OpenApi.Schema ->
  Either ValueSchemaError ValueType
parseValueSchema =
  error "TODO"

-- * Domain

type SchemaDict = HashMap Text OpenApi.Schema
