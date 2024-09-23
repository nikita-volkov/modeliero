module Modeliero.Sources.AsyncApi.Preludes.Parser
  ( module Modeliero.Sources.AsyncApi.Preludes.Parser,
    module Modeliero.Codegens.Haskell.Params,
    module Coalmine.Prelude,
    module Coalmine.ErrorReport,
  )
where

import Coalmine.ErrorReport (ConvertsToErrorReport (..), ErrorReport)
import Coalmine.Prelude hiding (label, parse)
import Data.OpenApi qualified as OpenApi
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.Codegens.Haskell.Params

data SchemaContext = SchemaContext
  { anonymizable :: Bool,
    dict :: HashMap Text OpenApi.Schema,
    config :: Config,
    contextReference :: Slug
  }

data Config = Config
  { defaultTextMaxLength :: Int
  }

-- * Parser patterns

type SchemaParser i o =
  SchemaContext -> i -> Either Json o

-- | Nest a parser in a context label and input for error-reports.
--
-- Essentially it is a combination of 'label' and 'assocWithInput'.
nest ::
  (ToJSON i) =>
  -- | Label.
  Text ->
  -- | Subparser.
  (SchemaContext -> i -> Either Json o) ->
  (SchemaContext -> i -> Either Json o)
nest label subparser schemaContext input =
  subparser schemaContext input
    & first (branchError label (toJSON input))

-- | Adapt a parser function with errors modeled in data-types convertible to JSON.
reportifyErrors ::
  (ToJSON e) =>
  (SchemaContext -> i -> Either e o) ->
  (SchemaContext -> i -> Either Json o)
reportifyErrors = (fmap . fmap . first) toJSON

-- * Result patterns

-- | Label a result.
--
-- Useful for associating results with stages.
label :: Text -> Either Json o -> Either Json o
label label =
  first (Json.tagged label)

-- | Associate the errors in the result with input.
assocWithInput :: (ToJSON i) => i -> Either Json o -> Either Json o
assocWithInput input =
  first \reason ->
    [ ("error", reason),
      ("input", toJSON input)
    ]
      & Json.assocList

-- * Error

branchError ::
  -- | Error tag.
  Text ->
  -- | Rendered input.
  Json ->
  -- | Reason.
  Json ->
  -- | Compiled error object.
  Json
branchError tag input reason =
  [ ("error", reason),
    ("input", input)
  ]
    & Json.assocList
    & Json.tagged tag
