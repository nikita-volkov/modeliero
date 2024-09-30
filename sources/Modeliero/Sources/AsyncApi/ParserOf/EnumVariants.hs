module Modeliero.Sources.AsyncApi.ParserOf.EnumVariants where

import Modeliero.Sources.AsyncApi.ParserOf.EnumVariant qualified as EnumVariant
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = [Json]

type Output = [EnumVariant]

parse :: SchemaContext -> Input -> Either Json Output
parse schemaContext input = assocWithInput input do
  traverse (EnumVariant.parse schemaContext) input
