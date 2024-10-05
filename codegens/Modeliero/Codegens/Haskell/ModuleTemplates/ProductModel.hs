{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- |
-- Integrates template to the Params model.
module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel
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
import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.ProductModelModule qualified as Template
import Modeliero.Codegens.Haskell.Params

data Params = Params
  { modelsNamespace :: [Text],
    instances :: Instances,
    name :: Slug,
    docs :: Text,
    fields :: [Field],
    anonymizable :: Bool
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
        instances = compileInstances params.instances,
        anonymizable = params.anonymizable
      }

-- * Helpers

-- Names of functions are in terms of the Template model.
-- I.e., what they produce.

compileInstances :: Instances -> Template.Instances
compileInstances instances =
  Template.Instances
    { aeson = instances.aeson & isJust,
      arbitrary = instances.arbitrary,
      anonymizable = instances.anonymizable,
      hashable = True
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
          if field.jsonName == ""
            then
              field.name & case jsonCasing of
                CamelCasing -> Slug.toLowerCamelCaseText
                SnakeCasing -> Slug.toSnakeCaseText
                KebabCasing -> Slug.toSpinalCaseText
            else
              field.jsonName,
        nullable = case field.type_ of
          MaybeValueType _ -> True
          _ -> False
      }

compileValueType :: [Text] -> ValueType -> InModule.InModule Text
compileValueType modelsNamespace = \case
  PlainValueType a -> compilePlainType modelsNamespace a
  MaybeValueType a -> do
    basePreludeQfr <- InModule.requestImport Imports.basePreludeRoot
    compileValueType modelsNamespace a
      & fmap (\itemType -> basePreludeQfr <> "Maybe (" <> itemType <> ")")
  VectorValueType a -> do
    vectorQfr <- InModule.requestImport Imports.vectorRoot
    compileValueType modelsNamespace a
      & fmap (\itemType -> vectorQfr <> "Vector (" <> itemType <> ")")

compilePlainType :: [Text] -> PlainType -> InModule.InModule Text
compilePlainType modelsNamespace = \case
  LocalPlainType nameSlug -> do
    let typeName = Slug.toUpperCamelCaseText nameSlug
    qfr <-
      InModule.requestImport
        InModule.Import
          { dependency = Nothing,
            name = foldMap (<> ".") modelsNamespace <> typeName
          }
    pure (qfr <> typeName)
  StandardPlainType a -> compileStandardType a
  CustomPlainType customType ->
    error ("TODO: " <> show customType)

compileStandardType :: StandardType -> InModule.InModule Text
compileStandardType standardType =
  case standardType of
    BoolStandardType ->
      InModule.requestImport Imports.basePreludeRoot
        & fmap (<> "Bool")
    IntStandardType ->
      InModule.requestImport Imports.basePreludeRoot
        & fmap (<> "Int")
    ScientificStandardType ->
      InModule.requestImport Imports.scientific
        & fmap (<> "Scientific")
    DoubleStandardType ->
      InModule.requestImport Imports.basePreludeRoot
        & fmap (<> "Double")
    TextStandardType ->
      InModule.requestImport Imports.textRoot
        & fmap (<> "Text")
    UuidStandardType ->
      InModule.requestImport Imports.uuidRoot
        & fmap (<> "UUID")
    EmailStandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "Email")
    IriStandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "Iri")
    HostnameStandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "Hostname")
    IpV4StandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "IpV4")
    IpV6StandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "IpV6")
    Iso8601DateTimeStandardType ->
      InModule.requestImport Imports.modelieroBaseRoot
        & fmap (<> "Iso8601DateTime")
