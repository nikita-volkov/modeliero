module Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.DataTypeDeclaration
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text

data Params = Params
  { name :: TextBlock,
    haddock :: Text,
    variants :: [Variant],
    derivings :: [TextBlock]
  }

data Variant = Variant
  { name :: TextBlock,
    type_ :: TextBlock,
    haddock :: Text
  }

compile :: Params -> TextBlock
compile params =
  mconcat
    [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
      "data " <> params.name,
      case compileConstructors params of
        [] -> mempty
        head : tail ->
          mconcat
            [ "\n  = " <> head,
              foldMap ("\n  | " <>) tail
            ],
      case params.derivings of
        [] -> mempty
        derivingSplices -> "\n  deriving (" <> TextBlock.intercalate ", " derivingSplices <> ")"
    ]

compileConstructors :: Params -> [TextBlock]
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
