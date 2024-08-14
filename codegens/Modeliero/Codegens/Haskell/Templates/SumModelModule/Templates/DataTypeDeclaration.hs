module Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.DataTypeDeclaration
  ( Params (..),
    Result,
    Variant (..),
    Derivings (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: TextBlock,
    haddock :: Text,
    variants :: [Variant],
    derivings :: Derivings
  }

type Result = InModule TextBlock

data Variant = Variant
  { name :: TextBlock,
    type_ :: TextBlock,
    haddock :: Text
  }

data Derivings = Derivings
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool
  }

compile :: Params -> Result
compile params = do
  derivingSplices <- compileDerivings params
  constructorSplices <- compileConstructors params
  pure
    ( mconcat
        [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
          "data " <> params.name,
          case constructorSplices of
            [] -> mempty
            head : tail ->
              mconcat
                [ "\n  = " <> head,
                  foldMap ("\n  | " <>) tail
                ],
          case derivingSplices of
            [] -> mempty
            _ -> "\n  deriving (" <> TextBlock.intercalate ", " derivingSplices <> ")"
        ]
    )

compileDerivings :: Params -> InModule [TextBlock]
compileDerivings params = do
  sequence
    $ catMaybes
      [ compileDeriving params.derivings.show "Show" (requestImport Imports.basePrelude),
        compileDeriving params.derivings.eq "Eq" (requestImport Imports.basePrelude),
        compileDeriving params.derivings.ord "Ord" (requestImport Imports.basePrelude),
        compileDeriving params.derivings.generic "Generic" (requestImport Imports.baseGenerics)
      ]
  where
    compileDeriving :: Bool -> Text -> InModule Text -> Maybe (InModule TextBlock)
    compileDeriving enabled name importRequest =
      if enabled
        then Just (importRequest <&> \qfr -> to qfr <> to name)
        else Nothing

compileConstructors :: Params -> InModule [TextBlock]
compileConstructors params =
  params.variants
    & fmap
      ( \variant ->
          mconcat
            [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) variant.haddock,
              variant.name,
              params.name,
              " ",
              variant.type_
            ]
      )
    & pure
