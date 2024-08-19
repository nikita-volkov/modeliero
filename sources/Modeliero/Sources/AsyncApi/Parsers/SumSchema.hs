module Modeliero.Sources.AsyncApi.Parsers.SumSchema where

import Data.OpenApi qualified as Input
import Modeliero.Sources.AsyncApi.Parsers.SumOneOf qualified as Parsers.SumOneOf
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

type Output = Parsers.SumOneOf.Output

data Error
  = NoOneOfError
  | SumOneOfError Parsers.SumOneOf.Error

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  oneOf <-
    input._schemaOneOf
      & \case
        Nothing -> Left NoOneOfError
        Just oneOf -> pure oneOf
  Parsers.SumOneOf.parse schemaContext oneOf
    & first SumOneOfError
