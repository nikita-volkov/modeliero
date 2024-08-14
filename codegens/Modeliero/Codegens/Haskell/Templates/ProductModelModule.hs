{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- |
-- Integrates template to the Params model.
module Modeliero.Codegens.Haskell.Templates.ProductModelModule
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder (TextBlock)
import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ProductModelModule qualified as Template

data Params = Params
  { modelsNamespace :: [Text],
    instances :: Instances,
    name :: Slug,
    docs :: Text,
    fields :: [Field]
  }

type Result = InModule.InModule TextBlock

compile :: Params -> Template.Result
compile params = do
  let jsonCasing = maybe CamelCasing (.casing) params.instances.aeson
  compiledFields <- forM params.fields $ compileField params.modelsNamespace jsonCasing
  Template.compile
    Template.Params
      { name = Slug.toUpperCamelCaseText params.name,
        haddock = params.docs,
        fields = compiledFields,
        instances = compileInstances params.instances
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
  type_ <- compileValueType modelsNamespace field.type_
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
          MaybeValueType _ -> True
          _ -> False
      }

compileValueType :: [Text] -> ValueType -> InModule.InModule Text
compileValueType modelsNamespace = \case
  PlainValueType plainType -> case plainType of
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
