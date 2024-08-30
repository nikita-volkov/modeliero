{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.ParserOf.ExtendedComponents where

import Cases qualified
import Coalmine.ErrorReport qualified as ErrorReport
import Coalmine.Prelude
import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.AsyncApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.Schema qualified as Parsers.TypeDeclarationSchema
import Modeliero.Sources.AsyncApi.Parsers.SumSchema qualified as Parsers.SumSchema
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.ExtendedComponents

type Output = [TypeDeclaration]

data Error
  = SumSchemaError Parsers.SumSchema.Error
  | SlugParsingError Text Text
  | TypeDeclarationSchemaError Text Parsers.TypeDeclarationSchema.Error
  deriving (Eq, Show, Generic)

instance ToJSON Error where
  toJSON = \case
    SumSchemaError sumSchemaError ->
      sumSchemaError
        & toJSON
        & Json.tagged "sum-schema"
    SlugParsingError input message ->
      [ ("input", toJSON input),
        ("message", toJSON message)
      ]
        & Json.assocList
        & Json.tagged "slug-parsing"
    TypeDeclarationSchemaError name reason ->
      [ ("name", toJSON name),
        ("reason", toJSON reason)
      ]
        & Json.assocList
        & Json.tagged "type-declaration-schema"

parse :: Config -> Input -> Either Error Output
parse config input = do
  typeDeclarations <-
    input.schemas
      & toList
      & traverse
        ( \(name, schemaInput) -> do
            let schemaContext =
                  SchemaContext
                    { anonymizable = schemaInput.anonymizable,
                      dict = input.schemas & fmap (.base),
                      config
                    }
            nameSlug <-
              name
                & Cases.spinalize
                & specialize
                & first (SlugParsingError name)
            typeDeclarationSchemaOutput <-
              Parsers.TypeDeclarationSchema.parse schemaContext schemaInput.base
                & first (TypeDeclarationSchemaError name)
            pure
              TypeDeclaration
                { name = nameSlug,
                  docs = typeDeclarationSchemaOutput.docs,
                  definition = typeDeclarationSchemaOutput.definition
                }
        )
  error "TODO"
