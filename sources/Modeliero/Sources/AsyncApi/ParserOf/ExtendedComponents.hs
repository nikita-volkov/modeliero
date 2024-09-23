module Modeliero.Sources.AsyncApi.ParserOf.ExtendedComponents where

import Cases qualified
import Modeliero.AsyncApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.OutlineSchema qualified as OutlineSchemaParser
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.ExtendedComponents

type Output = [TypeDeclaration]

type Error = Json

parse :: Config -> Input -> Either Error Output
parse config input = do
  typeDeclarations <-
    input.schemas
      & toList
      & traverse
        ( \(name, schemaInput) -> label name do
            contextReference <-
              name
                & Cases.spinalize
                & specialize
                & first toJSON
                & label "name"
                & assocWithInput name
            let schemaContext =
                  SchemaContext
                    { anonymizable = schemaInput.anonymizable,
                      dict = input.schemas & fmap (.base),
                      config,
                      contextReference
                    }
            schemaOutput <-
              nest "schema" OutlineSchemaParser.parse schemaContext schemaInput.base
            pure
              TypeDeclaration
                { name = contextReference,
                  docs = schemaOutput.docs,
                  definition = schemaOutput.definition
                }
        )
  pure typeDeclarations
