{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches #-}

module Modeliero.Codegens.Haskell.Templates.WrapperModelModule
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.CompilersOf.TypeSig qualified as CompilersOf.TypeSig
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.WrapperModelModule.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration

data Params = Params
  { modelsNamespace :: [Text],
    name :: Slug,
    docs :: Text,
    baseType :: Params.ValueType,
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name)
  decls <-
    (sequence . catMaybes)
      [ Just do
          baseType <- CompilersOf.TypeSig.fromValueType params.modelsNamespace params.baseType
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
                      generic = params.instances.generic,
                      hashable = params.instances.hashable
                    }
              }
      ]
  pure (TextBlock.intercalate "\n\n" decls)
