{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Modeliero.Codegens.Haskell.CompilersOf.TypeSig where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.ModuleTemplates.ProxyModel.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.Params qualified as Params

fromValueType :: [Text] -> Params.ValueType -> InModule Text
fromValueType modelsNamespace = \case
  Params.PlainValueType plainType -> fromPlainType modelsNamespace plainType
  Params.VectorValueType itemType -> do
    vectorQfr <- requestImport Imports.vectorRoot
    itemSig <- fromValueType modelsNamespace itemType
    pure
      [i|
        ${vectorQfr}Vector (${itemSig})
      |]

fromPlainType :: [Text] -> Params.PlainType -> InModule Text
fromPlainType modelsNamespace = \case
  Params.LocalPlainType nameSlug -> do
    let typeName = nameSlug & Slug.toUpperCamelCaseTextBuilder & to
    qfr <-
      requestImport
        Import
          { dependency = Nothing,
            name = to (foldMap ((<> ".") . to) modelsNamespace <> typeName)
          }
    pure (to qfr <> typeName)
  Params.StandardPlainType standardType ->
    fromStandardType standardType

fromStandardType :: Params.StandardType -> InModule Text
fromStandardType = \case
  Params.BoolStandardType ->
    requestImport Imports.basePreludeRoot <&> (<> "Bool")
  Params.DoubleStandardType ->
    requestImport Imports.basePreludeRoot <&> (<> "Double")
  Params.IntStandardType ->
    requestImport Imports.basePreludeRoot <&> (<> "Int")
  Params.ScientificStandardType ->
    requestImport Imports.scientific <&> (<> "Scientific")
  Params.EmailStandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "Email")
  Params.HostnameStandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "Hostname")
  Params.IpV4StandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "IpV4")
  Params.IpV6StandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "IpV6")
  Params.TextStandardType ->
    requestImport Imports.textRoot <&> (<> "Text")
  Params.UuidStandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "Uuid")
  Params.Iso8601DateTimeStandardType ->
    requestImport Imports.modelieroBaseRoot <&> (<> "Iso8601DateTime")
