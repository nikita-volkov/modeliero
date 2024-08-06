module Modeliero.Codegens.Haskell.Templates.ModelModule.Templates.Product.Templates.DataTypeDeclaration where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: TextBlock,
    haddock :: Text,
    fields :: [Field],
    derivings :: Derivings
  }

type Result = InModule TextBlock

data Field = Field
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
  derivingSplices <- compileDerivings params.derivings
  let fieldSplices =
        params.fields
          & fmap
            ( \field ->
                mconcat
                  [ filtered
                      (not . Text.null)
                      (flip mappend "\n" . Snippets.prefixHaddock)
                      field.haddock,
                    field.name,
                    " :: ",
                    field.type_
                  ]
            )
      recordDecl =
        mconcat
          [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
            Snippets.dataRecordDecl
              params.name
              params.name
              fieldSplices,
            case derivingSplices of
              [] -> mempty
              _ -> "\n  deriving (" <> TextBlock.intercalate ", " derivingSplices <> ")"
          ]
  pure recordDecl

compileDerivings :: Derivings -> InModule [TextBlock]
compileDerivings derivings = do
  sequence
    $ catMaybes
      [ compileDeriving derivings.show "Show" (requestImport Imports.basePrelude),
        compileDeriving derivings.eq "Eq" (requestImport Imports.basePrelude),
        compileDeriving derivings.ord "Ord" (requestImport Imports.basePrelude),
        compileDeriving derivings.ord "Generic" (requestImport Imports.baseGenerics)
      ]

compileDeriving :: Bool -> Text -> InModule Text -> Maybe (InModule TextBlock)
compileDeriving enabled name importRequest =
  if enabled
    then Just (importRequest <&> \qfr -> to qfr <> to name)
    else Nothing
