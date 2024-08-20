{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi where

import Coalmine.Prelude
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
import Modeliero.AsyncApi qualified as AsyncApi
import Modeliero.Codegens.Haskell.Params qualified as Codegen

-- | Lib API error.
data Error
  = YamlLoadingError SomeException
  | ParsingError
      -- | Message.
      Text
      -- | Details.
      [(Text, Json)]
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

load :: FilePath -> IO (Either Error Codegen.Model)
load path =
  AsyncApi.load path
    & fmap parseAsyncApi
    & handle (pure . Left . YamlLoadingError)

parseAsyncApi :: AsyncApi.AsyncApi -> Either Error Codegen.Model
parseAsyncApi =
  error "TODO"
