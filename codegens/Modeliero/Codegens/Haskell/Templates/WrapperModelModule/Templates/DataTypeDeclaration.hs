module Modeliero.Codegens.Haskell.Templates.WrapperModelModule.Templates.DataTypeDeclaration where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: TextBlock,
    haddock :: Text,
    baseType :: TextBlock,
    derivings :: Derivings
  }

type Result = InModule TextBlock

data Derivings = Derivings
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool,
    hashable :: Bool
  }

compile :: Params -> Result
compile params = do
  derivingStockSplices <- compileStockDerivings params.derivings
  derivingNewtypeSplices <- compileNewtypeDerivings params.derivings
  pure
    ( mconcat
        [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
          [j|
            newtype ${params.name} = ${params.name}
              { base :: ${params.baseType}
              }
          |],
          case derivingStockSplices of
            [] -> mempty
            _ -> "\n  deriving stock (" <> TextBlock.intercalate ", " derivingStockSplices <> ")",
          case derivingStockSplices of
            [] -> mempty
            _ -> "\n  deriving newtype (" <> TextBlock.intercalate ", " derivingNewtypeSplices <> ")"
        ]
    )

compileStockDerivings :: Derivings -> InModule [TextBlock]
compileStockDerivings derivings =
  [ compileDeriving derivings.show "Show" (requestImport Imports.basePrelude)
  ]
    & catMaybes
    & sequence

compileNewtypeDerivings :: Derivings -> InModule [TextBlock]
compileNewtypeDerivings derivings =
  [ compileDeriving derivings.eq "Eq" (requestImport Imports.basePrelude),
    compileDeriving derivings.ord "Ord" (requestImport Imports.basePrelude),
    compileDeriving derivings.generic "Generic" (requestImport Imports.baseGenerics),
    compileDeriving derivings.hashable "Hashable" (requestImport Imports.hashable)
  ]
    & catMaybes
    & sequence

compileDeriving :: Bool -> Text -> InModule Text -> Maybe (InModule TextBlock)
compileDeriving enabled name importRequest =
  if enabled
    then Just (importRequest <&> \qfr -> to qfr <> to name)
    else Nothing
