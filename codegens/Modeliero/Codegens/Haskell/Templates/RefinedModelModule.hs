{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.Templates.RefinedModelModule where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.ArbitraryInstance qualified as Templates.ArbitraryInstance
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.SpecialInstance qualified as Templates.SpecialInstance

data Params = Params
  { name :: Slug,
    docs :: Text,
    refinement :: Params.Refinement,
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name)
  decls <-
    (sequence . catMaybes)
      [ Just do
          baseType <- case params.refinement of
            Params.TextRefinement _ ->
              requestImport Imports.text <&> \qfr -> qfr <> "Text"
            Params.IntegerRefinement _ ->
              -- TODO: Add analysis on the bounds to determine whether it should be Integer or Int.
              requestImport Imports.basePrelude <&> \qfr -> qfr <> "Int"
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
                      generic = params.instances.generic
                    }
              }
      ]
  pure (TextBlock.intercalate "\n\n" decls)
