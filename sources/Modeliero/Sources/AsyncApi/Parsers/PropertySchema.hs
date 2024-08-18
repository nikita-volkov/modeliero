{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.PropertySchema where

import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

data Output = Output
  { docs :: Text,
    type_ :: ValueType
  }

data Error
  = RefError Text Input.Schema SchemaError
  | InlineError Input.Schema SchemaError
  | RefNotFoundError Text

data SchemaError
  = OneOfError
      -- | Offset.
      Int
      -- | Input.
      (Input.Referenced Input.Schema)
      OneOfError
  | NoDefinitionSchemaError

data OneOfError
  = -- | The sum one-of pattern was recognized, but there is an error parsing the sum.
    SumPatternOneOfError
      -- | Sum tag name.
      Text
      -- | Reason.
      SumError

data SumError

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  error "TODO"
  where
    parseSchema schema =
      asum
        [ schema._schemaOneOf
            & fmap parseSchemaOneOf
        ]
        & fromMaybe (Left NoDefinitionSchemaError)

    parseSchemaOneOf oneOf =
      error "TODO"
