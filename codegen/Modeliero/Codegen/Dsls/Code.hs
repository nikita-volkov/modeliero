{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegen.Dsls.Code where

import Coalmine.NumericVersion qualified as NumericVersion
import Coalmine.Prelude hiding (writeFile)
import CodegenKit.HaskellPackage.Contexts.Code qualified as Legacy
import CodegenKit.HaskellPackage.Contexts.Exp qualified as LegacyExp
import Data.Text qualified as Text
import Modeliero.Codegen.Dsls.Package

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

newtype Code = Code
  { legacy :: Legacy.Code
  }
  deriving newtype (Semigroup, Monoid)

data Import = Import
  { -- | Possible external dependency.
    -- Affects the Cabal-file.
    --
    -- Nothing means that the imported module is from the same package.
    dependency :: Maybe Dependency,
    name :: Text
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

splicing :: Code -> (TextBlock -> Code) -> Code
splicing code cont =
  Code
    { legacy = Legacy.splicing code.legacy \splice -> (cont splice).legacy
    }

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
export = Code . Legacy.export

reexport :: Import -> Code
reexport import_ =
  mappend (foldMap dependency import_.dependency)
    $ Code
    $ Legacy.reexportUnqualifiedModule import_.name

dependency :: Dependency -> Code
dependency dependency =
  Code
    $ Legacy.dependency
      dependency.name
      dependency.minVersion.head
      dependency.minVersion.tail
      dependency.maxVersion.head
      dependency.maxVersion.tail

groupedLegacyExp :: LegacyExp.Exp -> Code
groupedLegacyExp exp =
  Code
    { legacy =
        LegacyExp.toGroupedCode exp
    }

ungroupedLegacyExp :: LegacyExp.Exp -> Code
ungroupedLegacyExp exp =
  Code
    { legacy =
        LegacyExp.toUngroupedCode exp
    }
