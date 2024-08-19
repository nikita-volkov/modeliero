{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi.Parsers.ValueTypeSchema where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SchemaReferencedSchema qualified as Parsers.SchemaReferencedSchema
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

type Output = ValueType

data Error

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input =
  error "TODO"
