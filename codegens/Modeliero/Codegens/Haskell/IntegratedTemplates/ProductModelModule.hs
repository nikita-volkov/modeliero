{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- |
-- Integrates template to the Params model.
module Modeliero.Codegens.Haskell.IntegratedTemplates.ProductModelModule (compile) where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Template

compile :: [Text] -> Instances -> Slug -> Text -> [Field] -> Template.Result
compile modelsNamespace instances name haddock fields = do
  let jsonCasing = maybe CamelCasing (.casing) instances.aeson
  compiledFields <- forM fields $ compileField modelsNamespace jsonCasing
  Template.compile
    Template.Params
      { name = Slug.toUpperCamelCaseText name,
        haddock,
        fields = compiledFields,
        instances = compileInstances instances
      }

-- * Helpers

-- Names of functions are in terms of the Template model.
-- I.e., what they produce.

compileInstances :: Instances -> Template.Instances
compileInstances instances =
  Template.Instances
    { show = instances.show,
      eq = instances.eq,
      ord = instances.ord,
      generic = instances.generic,
      aeson = instances.aeson & isJust,
      arbitrary = instances.arbitrary
    }

compileField :: [Text] -> Casing -> Field -> InModule.InModule Template.Field
compileField modelsNamespace jsonCasing field = do
  type_ <- compileFieldType modelsNamespace field.type_
  pure
    Template.Field
      { name = field.name & Slug.toLowerCamelCaseText,
        type_,
        haddock = field.docs,
        jsonName =
          field.name & case jsonCasing of
            CamelCasing -> Slug.toLowerCamelCaseText
            SnakeCasing -> Slug.toSnakeCaseText
            KebabCasing -> Slug.toSpinalCaseText,
        nullable = case field.type_ of
          MaybeFieldType _ -> True
          _ -> False
      }

compileFieldType :: [Text] -> FieldType -> InModule.InModule Text
compileFieldType modelsNamespace = \case
  PlainFieldType plainType -> case plainType of
    LocalPlainType nameSlug -> do
      let typeName = Slug.toUpperCamelCaseText nameSlug
      qfr <-
        InModule.requestImport
          InModule.Import
            { dependency = Nothing,
              name = foldMap (<> ".") modelsNamespace <> typeName
            }
      pure (qfr <> typeName)
    StandardPlainType standardType -> case standardType of
      BoolStandardType -> do
        qfr <- InModule.requestImport Imports.basePrelude
        pure (qfr <> "Bool")