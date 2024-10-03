{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.ModuleTemplates.SumModel
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
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.AnonymizableInstance qualified as Templates.AnonymizableInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.ArbitraryInstance qualified as Templates.ArbitraryInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.FromJsonInstance qualified as Templates.FromJsonInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.HashableInstance qualified as Templates.HashableInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.ToJsonInstance qualified as Templates.ToJsonInstance
import Modeliero.Codegens.Haskell.Params qualified as Params

data Params = Params
  { modelsNamespace :: [Text],
    name :: Slug,
    docs :: Text,
    variants :: [Params.Variant],
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name <> " (..)")
  decls <-
    (sequence . catMaybes)
      [ dataTypeDecl,
        hashableDecl,
        toJsonDecl,
        fromJsonDecl,
        arbitraryDecl,
        anonymizableDecl
      ]
  pure (TextBlock.intercalate "\n\n" decls)
  where
    dataTypeDecl =
      Just do
        variants <- for params.variants \variant -> do
          type_ <-
            CompilersOf.TypeSig.fromValueType params.modelsNamespace variant.type_
              & fmap to
          pure
            Templates.DataTypeDeclaration.Variant
              { name =
                  variant.name
                    & Slug.toUpperCamelCaseTextBuilder
                    & to,
                haddock = variant.docs,
                type_
              }
        stockDerivings <- compileDerivings params
        pure
          ( Templates.DataTypeDeclaration.compile
              Templates.DataTypeDeclaration.Params
                { name = params.name & Slug.toUpperCamelCaseText & to,
                  haddock = params.docs,
                  variants,
                  stockDerivings
                }
          )

    hashableDecl =
      Just do
        hashableQfr <- requestImport Imports.hashable
        pure
          ( Templates.HashableInstance.compile
              Templates.HashableInstance.Params
                { name = params.name & Slug.toUpperCamelCaseText & to,
                  variants =
                    params.variants
                      & fmap
                        ( \variant ->
                            Templates.HashableInstance.Variant
                              { name =
                                  variant.name
                                    & flip mappend params.name
                                    & Slug.toUpperCamelCaseTextBuilder
                                    & to,
                                memberNames =
                                  variant.name
                                    & Slug.toLowerCamelCaseTextBuilder
                                    & to
                                    & pure
                              }
                        ),
                  hashableQfr =
                    hashableQfr & to
                }
          )

    toJsonDecl =
      params.instances.aeson <&> \aesonParams -> do
        aesonQfr <- to <$> requestImport Imports.aeson
        aesonKeyMapQfr <- to <$> requestImport Imports.aesonKeyMap
        pure
          ( Templates.ToJsonInstance.compile
              Templates.ToJsonInstance.Params
                { aesonQfr,
                  aesonKeyMapQfr,
                  name =
                    params.name
                      & Slug.toUpperCamelCaseText
                      & to,
                  variants =
                    params.variants
                      & fmap
                        ( \variant ->
                            Templates.ToJsonInstance.Variant
                              { constructorName =
                                  variant.name
                                    & Slug.toUpperCamelCaseText
                                    & to,
                                varName =
                                  variant.name
                                    & Slug.toLowerCamelCaseText
                                    & to,
                                jsonName =
                                  if Text.null variant.jsonName
                                    then
                                      variant.name
                                        & case aesonParams.casing of
                                          Params.CamelCasing -> Slug.toLowerCamelCaseText
                                          Params.SnakeCasing -> Slug.toSnakeCaseText
                                          Params.KebabCasing -> Slug.toSpinalCaseText
                                        & to
                                    else variant.jsonName,
                                memberNames =
                                  variant.name
                                    & Slug.toLowerCamelCaseTextBuilder
                                    & to
                                    & pure
                              }
                        )
                }
          )

    fromJsonDecl =
      params.instances.aeson <&> \aesonParams -> do
        aesonQfr <- to <$> requestImport Imports.aeson
        aesonKeyMapQfr <- to <$> requestImport Imports.aesonKeyMap
        aesonTypesQfr <- to <$> requestImport Imports.aesonTypes
        pure
          ( Templates.FromJsonInstance.compile
              Templates.FromJsonInstance.Params
                { aesonQfr,
                  aesonKeyMapQfr,
                  aesonTypesQfr,
                  name =
                    params.name
                      & Slug.toUpperCamelCaseText,
                  variants =
                    params.variants
                      & fmap
                        ( \variant ->
                            Templates.FromJsonInstance.Variant
                              { constructorName =
                                  (variant.name <> params.name)
                                    & Slug.toUpperCamelCaseText,
                                varName =
                                  variant.name
                                    & Slug.toLowerCamelCaseText,
                                jsonName =
                                  if Text.null variant.jsonName
                                    then
                                      variant.name
                                        & case aesonParams.casing of
                                          Params.CamelCasing -> Slug.toLowerCamelCaseText
                                          Params.SnakeCasing -> Slug.toSnakeCaseText
                                          Params.KebabCasing -> Slug.toSpinalCaseText
                                    else
                                      variant.jsonName,
                                memberNames =
                                  variant.name
                                    & Slug.toLowerCamelCaseText
                                    & pure
                              }
                        )
                }
          )

    arbitraryDecl =
      if params.instances.arbitrary
        then Just do
          quickCheckArbitraryQfr <- to <$> requestImport Imports.quickCheckArbitrary
          quickCheckGenQfr <- to <$> requestImport Imports.quickCheckGen
          pure
            ( Templates.ArbitraryInstance.compile
                Templates.ArbitraryInstance.Params
                  { quickCheckArbitraryQfr,
                    quickCheckGenQfr,
                    name =
                      params.name
                        & Slug.toUpperCamelCaseText
                        & to,
                    variants =
                      params.variants
                        & fmap
                          ( \variant ->
                              Templates.ArbitraryInstance.Variant
                                { constructorName =
                                    (variant.name <> params.name)
                                      & Slug.toUpperCamelCaseText
                                      & to,
                                  memberNames =
                                    variant.name
                                      & Slug.toLowerCamelCaseText
                                      & to
                                      & pure
                                }
                          )
                  }
            )
        else Nothing

    anonymizableDecl =
      if params.instances.anonymizable
        then Just do
          anonymizableQfr <- to <$> requestImport Imports.anonymizable
          pure
            ( Templates.AnonymizableInstance.compile
                Templates.AnonymizableInstance.Params
                  { anonymizableQfr,
                    name =
                      params.name
                        & Slug.toUpperCamelCaseText
                        & to,
                    variants =
                      params.variants
                        & fmap
                          ( \variant ->
                              Templates.AnonymizableInstance.Variant
                                { constructorName =
                                    (variant.name <> params.name)
                                      & Slug.toUpperCamelCaseText
                                      & to,
                                  fields =
                                    [ Templates.AnonymizableInstance.VariantField
                                        { name =
                                            variant.name
                                              & Slug.toLowerCamelCaseText
                                              & to,
                                          anonymizable = variant.anonymizable
                                        }
                                    ]
                                }
                          )
                  }
            )
        else Nothing

compileDerivings :: Params -> InModule [TextBlock]
compileDerivings _params = do
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
