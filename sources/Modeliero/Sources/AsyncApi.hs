{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApi where

import Coalmine.Prelude
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi qualified as OpenApi
import Modeliero.AsyncApi qualified as AsyncApi
import Modeliero.Codegens.Haskell.Params qualified as Codegen

-- | Lib API error.
data Error = Error
  { code :: Int,
    message :: Text,
    details :: [(Text, Json)]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

load :: FilePath -> IO (Either Error Codegen.Model)
load =
  error "TODO"

parseYamlText :: Text -> Either Error Codegen.Model
parseYamlText =
  error "TODO"
