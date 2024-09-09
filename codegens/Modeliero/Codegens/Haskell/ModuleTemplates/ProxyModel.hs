{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches #-}

module Modeliero.Codegens.Haskell.ModuleTemplates.ProxyModel
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.CompilersOf.IsLiteral qualified as CompilersOf.IsLiteral
import Modeliero.Codegens.Haskell.CompilersOf.TypeSig qualified as CompilersOf.TypeSig
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.ModuleTemplates.ProxyModel.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.ModuleTemplates.ProxyModel.Templates.IsomorpicToInstances qualified as Templates.IsomorpicToInstances
import Modeliero.Codegens.Haskell.Params qualified as Params

data Params = Params
  { modelsNamespace :: [Text],
    name :: Slug,
    docs :: Text,
    baseType :: Params.ValueType,
    instances :: Params.Instances,
    forceAnonymization :: Bool
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name)
  baseType <- CompilersOf.TypeSig.fromValueType params.modelsNamespace params.baseType
  decls <-
    (sequence . catMaybes)
      [ Just do
          Templates.DataTypeDeclaration.compile
            Templates.DataTypeDeclaration.Params
              { name = params.name & Slug.toUpperCamelCaseText & to,
                haddock = params.docs,
                baseType = baseType & to,
                derivings =
                  Templates.DataTypeDeclaration.Derivings
                    { show = params.instances.show,
                      eq = params.instances.eq,
                      ord = params.instances.ord,
                      generic = False,
                      hashable = params.instances.hashable,
                      arbitrary = params.instances.arbitrary,
                      aeson = params.instances.aeson & isJust,
                      anonymizable = params.instances.anonymizable,
                      literal = params.baseType & CompilersOf.IsLiteral.valueType
                    }
              },
        Just do
          Templates.IsomorpicToInstances.compile
            Templates.IsomorpicToInstances.Params
              { name = params.name & Slug.toUpperCamelCaseText & to,
                baseType = baseType
              }
      ]
  pure (TextBlock.intercalate "\n\n" decls)
