{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.ModuleTemplates.EnumModel
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.CompilersOf.TypeSig qualified as CompilersOf.TypeSig
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.SnippetTemplates qualified as Templates

data Params = Params
  { name :: Slug,
    docs :: Text,
    variants :: [Params.EnumVariant],
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name <> " (..)")
  decls <-
    (sequence . catMaybes)
      [ Just (compileDataTypeDecl params)
      ]
  pure (TextBlock.intercalate "\n\n" decls)

compileDataTypeDecl :: Params -> InModule TextBlock
compileDataTypeDecl params = do
  stockDerivings <- compileStockDerivings params
  Templates.DataDeclaration
    { name = params.name & Slug.toUpperCamelCaseText & to,
      haddock =
        params.docs
          & guarded (not . Text.null)
          & fmap Templates.PrefixHaddock,
      variants =
        params.variants
          & fmap
            ( \variant ->
                Templates.DataDeclarationVariant
                  { name =
                      variant.slug
                        & flip mappend params.name
                        & Slug.toUpperCamelCaseTextBuilder
                        & to,
                    haddock = Nothing,
                    members = []
                  }
            ),
      stockDerivings
    }
    & toBroadBuilder
    & pure

compileStockDerivings :: Params -> InModule [TextBlock]
compileStockDerivings _params = do
  sequence
    $ catMaybes
      [ compileDeriving True "Show" Imports.basePreludeRoot,
        compileDeriving True "Read" Imports.basePreludeRoot,
        compileDeriving True "Eq" Imports.basePreludeRoot,
        compileDeriving True "Ord" Imports.basePreludeRoot
      ]
  where
    compileDeriving :: Bool -> Text -> Import -> Maybe (InModule TextBlock)
    compileDeriving enabled name import_ =
      if enabled
        then Just (requestImport import_ <&> \qfr -> to qfr <> to name)
        else Nothing
