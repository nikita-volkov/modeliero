{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Dsls.Code where

import Coalmine.NumericVersion qualified as NumericVersion
import Coalmine.Prelude hiding (writeFile)
import CodegenKit.HaskellPackage.Contexts.Code qualified as Legacy
import Data.Text qualified as Text
import Modeliero.Dsls.Package

compileCodeModule ::
  [Text] ->
  [(Text, Text)] ->
  Code ->
  Module
compileCodeModule name importAliases code =
  Module
    { name,
      dependencies = compileCodeDependencies code,
      content = compileCodeContent name importAliases code
    }

compileCodeDependencies :: Code -> [Dependency]
compileCodeDependencies =
  error "TODO"

compileCodeContent :: [Text] -> [(Text, Text)] -> Code -> Text
compileCodeContent namespace importAliases code =
  Legacy.toModuleText
    Legacy.ModuleConfig
      { namespace,
        importAliases,
        importRemappings = []
      }
    Legacy.Preferences
      { strictData = False,
        overloadedRecordDot = True,
        importQualifiedPost = True
      }
    code.legacy

data Code = Code
  { legacy :: Legacy.Code
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

importing :: Import -> (Text -> Code) -> Code
importing import_ cont =
  Code
    { legacy
    }
  where
    legacy =
      registerDependency <> importing
      where
        registerDependency =
          flip foldMap import_.dependency \dependency ->
            Legacy.dependency
              dependency.name
              dependency.minVersion.head
              dependency.minVersion.tail
              dependency.maxVersion.head
              dependency.maxVersion.tail
        importing =
          Legacy.importingModule import_.name ((.legacy) . cont . qualifier)
          where
            qualifier moduleName =
              if Text.null moduleName
                then mempty
                else (moduleName <> ".")

textBlock :: TextBlock -> Code
textBlock block =
  Code
    { legacy = Legacy.splice block
    }

-- | Register export.
export :: Text -> Code
export name =
  error "TODO"
