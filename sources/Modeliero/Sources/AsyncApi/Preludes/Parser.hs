module Modeliero.Sources.AsyncApi.Preludes.Parser
  ( module Modeliero.Sources.AsyncApi.Preludes.Parser,
    module Modeliero.Codegens.Haskell.Params,
    module Coalmine.Prelude,
  )
where

import Coalmine.Prelude
import Data.OpenApi qualified as OpenApi
import Modeliero.Codegens.Haskell.Params

type SchemaParser i e o =
  SchemaContext -> i -> Either e o

data SchemaContext = SchemaContext
  { anonymizable :: Bool,
    dict :: HashMap Text OpenApi.Schema
  }
