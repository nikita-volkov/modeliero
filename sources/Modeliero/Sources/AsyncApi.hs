module Modeliero.Sources.AsyncApi
  ( module Modeliero.Sources.AsyncApi,
    ParserPrelude.Config (..),
  )
where

import Coalmine.Prelude
import Modeliero.AsyncApi qualified as AsyncApi
import Modeliero.Codegens.Haskell.Params qualified as Codegen
import Modeliero.Sources.AsyncApi.ParserOf.AsyncApi qualified as ParserOf.AsyncApi
import Modeliero.Sources.AsyncApi.Preludes.Parser qualified as ParserPrelude

-- | Lib API error.
data Error
  = YamlLoadingError SomeException
  | ParsingError Json
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

instance ToJSON Error where
  toJSON = \case
    YamlLoadingError someException ->
      someException
        & displayException
        & toJSON
        & ("yaml-loading",)
        & pure
        & fromList
        & ObjectJson
    ParsingError report ->
      fromList
        [ ("parsing", report)
        ]
        & ObjectJson

load :: ParserPrelude.Config -> FilePath -> IO (Either Error [Codegen.TypeDeclaration])
load config path =
  AsyncApi.load path
    & fmap (parseAsyncApi config)
    & handle (pure . Left . YamlLoadingError)

parseAsyncApi :: ParserPrelude.Config -> AsyncApi.AsyncApi -> Either Error [Codegen.TypeDeclaration]
parseAsyncApi config =
  first (ParsingError . toJSON)
    . ParserOf.AsyncApi.parse config
