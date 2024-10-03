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
      [ Just (compileDataTypeDecl params),
        compileHashableInstance params,
        compileLiteralInstance params,
        compileArbitraryInstance params
      ]
  pure (TextBlock.intercalate "\n\n" decls)

compileDataTypeDecl :: Params -> InModule TextBlock
compileDataTypeDecl params = do
  stockDerivings <- compileStockDerivings params
  basePreludeQfr <- requestImport Imports.basePreludeRoot
  aesonQfr <- requestImport Imports.aeson
  modelieroBaseQfr <- requestImport Imports.modelieroBaseRoot
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
      stockDerivings,
      derivingVia =
        [ Templates.DerivingVia
            { derivings =
                [ basePreludeQfr <> "IsString",
                  basePreludeQfr <> "Read",
                  basePreludeQfr <> "Show",
                  aesonQfr <> "ToJSON",
                  aesonQfr <> "FromJSON",
                  aesonQfr <> "ToJSONKey",
                  aesonQfr <> "FromJSONKey"
                ]
                  & fmap to
                  & Templates.ParensList,
              viaSig =
                [ modelieroBaseQfr,
                  "ViaLiteral ",
                  params.name
                    & Slug.toUpperCamelCaseText
                ]
                  & mconcat
                  & to
            },
          Templates.DerivingVia
            { derivings =
                [ modelieroBaseQfr <> "Anonymizable"
                ]
                  & fmap to
                  & Templates.ParensList,
              viaSig =
                [ modelieroBaseQfr,
                  "ViaEnum ",
                  params.name
                    & Slug.toUpperCamelCaseText
                ]
                  & mconcat
                  & to
            }
        ]
    }
    & toBroadBuilder
    & pure

compileHashableInstance :: Params -> Maybe (InModule TextBlock)
compileHashableInstance params = Just do
  hashableQfr <- to <$> requestImport Imports.hashable
  Templates.AdtHashableInstance
    { name = params.name & Slug.toUpperCamelCaseText & to,
      hashableQfr = hashableQfr,
      saltPattern = saltVarName,
      variants =
        params.variants
          & zip (enumFrom 0)
          & fmap
            ( \(index, variant) ->
                Templates.AdtHashableInstanceConstructor
                  { name =
                      variant.slug
                        & flip mappend params.name
                        & Slug.toUpperCamelCaseTextBuilder
                        & to,
                    memberNames = [],
                    hashableQfr = hashableQfr,
                    saltExp = saltVarName,
                    index
                  }
            )
    }
    & toBroadBuilder
    & pure
  where
    saltVarName = "salt"

compileLiteralInstance :: Params -> Maybe (InModule TextBlock)
compileLiteralInstance params =
  Just do
    literalQfr <- to <$> requestImport Imports.modelieroBaseRoot
    attoparsecQfr <- to <$> requestImport Imports.attoparsecText
    Templates.EnumLiteralInstance
      { name = params.name & Slug.toUpperCamelCaseText & to,
        literalQfr,
        attoparsecQfr,
        variants =
          params.variants
            & fmap
              ( \variant ->
                  ( variant.slug
                      & flip mappend params.name
                      & Slug.toUpperCamelCaseText
                      & to,
                    variant.jsonName
                  )
              )
      }
      & toBroadBuilder
      & pure

compileArbitraryInstance :: Params -> Maybe (InModule TextBlock)
compileArbitraryInstance params =
  Just do
    basePreludeQfr <- to <$> requestImport Imports.basePreludeRoot
    quickCheckArbitraryQfr <- to <$> requestImport Imports.quickCheckArbitrary
    quickCheckGenQfr <- to <$> requestImport Imports.quickCheckGen
    Templates.EnumArbitraryInstance
      { name = params.name & Slug.toUpperCamelCaseText & to,
        quickCheckArbitraryQfr,
        quickCheckGenQfr,
        basePreludeQfr
      }
      & toBroadBuilder
      & pure

compileStockDerivings :: Params -> InModule [TextBlock]
compileStockDerivings _params = do
  sequence
    $ catMaybes
      [ compileDeriving True "Eq" Imports.basePreludeRoot,
        compileDeriving True "Ord" Imports.basePreludeRoot,
        compileDeriving True "Enum" Imports.basePreludeRoot,
        compileDeriving True "Bounded" Imports.basePreludeRoot
      ]
  where
    compileDeriving :: Bool -> Text -> Import -> Maybe (InModule TextBlock)
    compileDeriving enabled name import_ =
      if enabled
        then Just (requestImport import_ <&> \qfr -> to qfr <> to name)
        else Nothing
