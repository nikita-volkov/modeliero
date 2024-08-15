{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.Templates.SumModelModule
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.HashableInstance qualified as Templates.HashableInstance
import Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.ToJsonInstance qualified as Templates.ToJsonInstance

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
      [ Just do
          variants <- for params.variants \variant -> do
            type_ <- compileValueType params.modelsNamespace variant.type_
            pure
              Templates.DataTypeDeclaration.Variant
                { name =
                    variant.name
                      & Slug.toUpperCamelCaseTextBuilder
                      & to,
                  haddock = variant.docs,
                  type_
                }
          derivings <- compileDerivings params
          pure
            ( Templates.DataTypeDeclaration.compile
                Templates.DataTypeDeclaration.Params
                  { name = params.name & Slug.toUpperCamelCaseText & to,
                    haddock = params.docs,
                    variants,
                    derivings
                  }
            ),
        if params.instances.hashable
          then Just do
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
          else Nothing,
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
                                    variant.name
                                      & case aesonParams.casing of
                                        Params.CamelCasing -> Slug.toLowerCamelCaseText
                                        Params.SnakeCasing -> Slug.toSnakeCaseText
                                        Params.KebabCasing -> Slug.toSpinalCaseText
                                      & to,
                                  memberNames =
                                    variant.name
                                      & Slug.toLowerCamelCaseTextBuilder
                                      & to
                                      & pure
                                }
                          )
                  }
            )
      ]
  pure (TextBlock.intercalate "\n\n" decls)

compileDerivings :: Params -> InModule [TextBlock]
compileDerivings params = do
  sequence
    $ catMaybes
      [ compileDeriving params.instances.show "Show" Imports.basePrelude,
        compileDeriving params.instances.eq "Eq" Imports.basePrelude,
        compileDeriving params.instances.ord "Ord" Imports.basePrelude,
        compileDeriving params.instances.generic "Generic" Imports.baseGenerics
      ]
  where
    compileDeriving :: Bool -> Text -> Import -> Maybe (InModule TextBlock)
    compileDeriving enabled name import_ =
      if enabled
        then Just (requestImport import_ <&> \qfr -> to qfr <> to name)
        else Nothing

compileValueType :: [Text] -> Params.ValueType -> InModule TextBlock
compileValueType modelsNamespace = \case
  Params.PlainValueType plainType -> case plainType of
    Params.LocalPlainType nameSlug -> do
      let typeName = nameSlug & Slug.toUpperCamelCaseTextBuilder & to
      qfr <-
        requestImport
          Import
            { dependency = Nothing,
              name = to (foldMap ((<> ".") . to) modelsNamespace <> typeName)
            }
      pure (to qfr <> typeName)
    Params.StandardPlainType standardType -> case standardType of
      Params.BoolStandardType -> do
        qfr <- requestImport Imports.basePrelude
        pure (to qfr <> "Bool")
