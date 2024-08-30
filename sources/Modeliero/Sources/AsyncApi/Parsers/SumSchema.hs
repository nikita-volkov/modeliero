module Modeliero.Sources.AsyncApi.Parsers.SumSchema where

import Data.OpenApi qualified as Input
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Sources.AsyncApi.Parsers.SumOneOf qualified as Parsers.SumOneOf
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.Schema

type Output = Parsers.SumOneOf.Output

data Error
  = NoOneOfError
  | SumOneOfError Parsers.SumOneOf.Error
  deriving (Eq, Show, Generic)

instance ToJSON Error where
  toJSON = \case
    NoOneOfError ->
      "no-one-of"
    SumOneOfError someOneOfError ->
      someOneOfError
        & toJSON
        & Json.tagged "sum-one-of"

parse :: SchemaContext -> Input -> Either Error Output
parse schemaContext input = do
  oneOf <-
    input._schemaOneOf
      & \case
        Nothing -> Left NoOneOfError
        Just oneOf -> pure oneOf
  Parsers.SumOneOf.parse schemaContext oneOf
    & first SumOneOfError
