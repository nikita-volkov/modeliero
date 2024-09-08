module Modeliero.Codegens.Haskell.Templates.ProxyModelModule.Templates.DataTypeDeclaration where

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
    hashable :: Bool,
    arbitrary :: Bool,
    aeson :: Bool,
    literal :: Bool,
    anonymizable :: Bool
  }

compile :: Params -> Result
compile params = do
  derivingStockSplices <- compileStockDerivings params
  derivingNewtypeSplices <- compileNewtypeDerivings params
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
          case derivingNewtypeSplices of
            [] -> mempty
            _ ->
              if length derivingNewtypeSplices <= 3
                then "\n  deriving newtype (" <> TextBlock.intercalate ", " derivingNewtypeSplices <> ")"
                else
                  let derivingsSplice =
                        derivingNewtypeSplices
                          & TextBlock.intercalate ",\n"
                          & TextBlock.indent 2
                   in (TextBlock.indent 2)
                        [i|
                          
                          deriving newtype
                            ( ${derivingsSplice}
                            )
                        |]
        ]
    )

compileStockDerivings :: Params -> InModule [TextBlock]
compileStockDerivings _ =
  []
    & catMaybes
    & sequence

compileNewtypeDerivings :: Params -> InModule [TextBlock]
compileNewtypeDerivings params =
  [ compileDeriving params.derivings.show "Show" Imports.basePreludeRoot,
    compileDeriving params.derivings.eq "Eq" Imports.basePreludeRoot,
    compileDeriving params.derivings.ord "Ord" Imports.basePreludeRoot,
    compileDeriving params.derivings.generic "Generic" Imports.baseGenerics,
    compileDeriving params.derivings.hashable "Hashable" Imports.hashable,
    compileDeriving params.derivings.arbitrary "Arbitrary" Imports.quickCheckArbitrary,
    compileDeriving params.derivings.aeson "ToJSON" Imports.aeson,
    compileDeriving params.derivings.aeson "FromJSON" Imports.aeson,
    compileDeriving params.derivings.aeson "ToJSONKey" Imports.aeson,
    compileDeriving params.derivings.aeson "FromJSONKey" Imports.aeson,
    compileDeriving params.derivings.literal "Literal" Imports.modelieroBaseRoot,
    compileDeriving params.derivings.anonymizable "Anonymizable" Imports.modelieroBaseRoot
  ]
    & catMaybes
    & sequence

compileDeriving :: Bool -> Text -> Import -> Maybe (InModule TextBlock)
compileDeriving enabled name import_ =
  if enabled
    then Just (requestImport import_ <&> \qfr -> to qfr <> to name)
    else Nothing
