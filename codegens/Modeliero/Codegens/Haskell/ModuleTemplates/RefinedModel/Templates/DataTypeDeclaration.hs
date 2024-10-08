module Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.DataTypeDeclaration where

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
    read :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool,
    hashable :: Bool
  }

compile :: Params -> Result
compile params = do
  derivingSplices <- compileDerivings params.derivings
  pure
    ( mconcat
        [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
          [j|
            newtype ${params.name} = ${params.name}
              { base :: ${params.baseType}
              }
          |],
          case derivingSplices of
            [] -> mempty
            _ -> "\n  deriving (" <> TextBlock.intercalate ", " derivingSplices <> ")"
        ]
    )

compileDerivings :: Derivings -> InModule [TextBlock]
compileDerivings derivings = do
  sequence
    $ catMaybes
      [ compileDeriving derivings.show "Show" (requestImport Imports.basePreludeRoot),
        compileDeriving derivings.read "Read" (requestImport Imports.basePreludeRoot),
        compileDeriving derivings.eq "Eq" (requestImport Imports.basePreludeRoot),
        compileDeriving derivings.ord "Ord" (requestImport Imports.basePreludeRoot),
        compileDeriving derivings.generic "Generic" (requestImport Imports.baseGenerics),
        compileDeriving derivings.hashable "Hashable" (requestImport Imports.hashable)
      ]

compileDeriving :: Bool -> Text -> InModule Text -> Maybe (InModule TextBlock)
compileDeriving enabled name importRequest =
  if enabled
    then Just (importRequest <&> \qfr -> to qfr <> to name)
    else Nothing
