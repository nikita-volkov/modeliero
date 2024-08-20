{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.TypeDeclarationsComponents where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SumSchema qualified as Parsers.SumSchema
import Modeliero.Sources.AsyncApi.Parsers.SumVariantSchema qualified as Parsers.SumVariantSchema
import Modeliero.Sources.AsyncApi.Parsers.TypeDeclarationSchema qualified as Parsers.TypeDeclarationSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Components

type Output = [TypeDeclaration]

data Error
  = SumSchemaError Parsers.SumSchema.Error
  | SlugParsingError Text Text
  | TypeDeclarationSchemaError Parsers.TypeDeclarationSchema.Error

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  typeDeclarations <-
    input._componentsSchemas
      & toList
      & traverse
        ( \(name, schemaInput) -> do
            nameSlug <-
              specialize name
                & first (SlugParsingError name)
            typeDeclarationSchemaOutput <-
              Parsers.TypeDeclarationSchema.parse schemaContext schemaInput
                & first TypeDeclarationSchemaError
            pure
              TypeDeclaration
                { name = nameSlug,
                  docs = typeDeclarationSchemaOutput.docs,
                  definition = typeDeclarationSchemaOutput.definition
                }
        )
  error "TODO"
