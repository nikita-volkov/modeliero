{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi
  ( module Modeliero.Sources.AsyncApi,
    ParserPrelude.Config (..),
  )
where

import Coalmine.Prelude
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
import Modeliero.AsyncApi qualified as AsyncApi
import Modeliero.Codegens.Haskell.Params qualified as Codegen
import Modeliero.Sources.AsyncApi.ParserOf.AsyncApi qualified as ParserOf.AsyncApi
import Modeliero.Sources.AsyncApi.Preludes.Parser qualified as ParserPrelude

-- | Lib API error.
data Error
  = YamlLoadingError SomeException
  | ParsingError Text
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

load :: ParserPrelude.Config -> FilePath -> IO (Either Error [Codegen.TypeDeclaration])
load config path =
  AsyncApi.load path
    & fmap (parseAsyncApi config)
    & handle (pure . Left . YamlLoadingError)

parseAsyncApi :: ParserPrelude.Config -> AsyncApi.AsyncApi -> Either Error [Codegen.TypeDeclaration]
parseAsyncApi config =
  first (ParsingError . fromString . show)
    . ParserOf.AsyncApi.parse config
