{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Dsls.Code where

import Coalmine.Prelude hiding (writeFile)
import Data.Text qualified as Text
import Modeliero.Dsls.Package

compileCodeModule :: [Text] -> Code -> Module
compileCodeModule name code =
  Module
    { name,
      dependencies = compileCodeDependencies code,
      content = compileCodeContent (Text.intercalate "." name) code
    }

compileCodeDependencies :: Code -> [Dependency]
compileCodeDependencies =
  error "TODO"

compileCodeContent :: Text -> Code -> Text
compileCodeContent =
  error "TODO"

data Code = Code
  { importRequests :: HashMap Import Text,
    -- | Function on prefix namespace and import resolver.
    printer :: [Text] -> (Import -> Text) -> TextBlock
  }

data Import = Import
  { -- | Possible external dependency.
    -- Affects the Cabal-file.
    --
    -- Nothing means that the imported module is from the same package.
    dependency :: Maybe Dependency,
    name :: Text
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

import_ ::
  Import ->
  -- | Code with the import request registered and
  -- either an empty splice or a qualification prefix ending with dot.
  Code
import_ =
  error "TODO"

importing :: Import -> (Text -> Code) -> Code
importing =
  error "TODO"

textBlock :: TextBlock -> Code
textBlock block =
  Code
    { importRequests = mempty,
      printer = \_ _ -> block
    }
