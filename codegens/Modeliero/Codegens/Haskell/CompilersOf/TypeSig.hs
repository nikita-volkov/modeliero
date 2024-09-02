{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches #-}

module Modeliero.Codegens.Haskell.CompilersOf.TypeSig where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.WrapperModelModule.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration

fromValueType :: [Text] -> Params.ValueType -> InModule Text
fromValueType modelsNamespace = \case
  Params.PlainValueType plainType -> fromPlainType modelsNamespace plainType

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
    requestImport Imports.basePrelude <&> (<> "Bool")
  Params.EmailStandardType ->
    requestImport Imports.modelieroBaseTypes <&> (<> "Email")
  Params.HostnameStandardType ->
    requestImport Imports.modelieroBaseTypes <&> (<> "Hostname")
  Params.IpV4StandardType ->
    requestImport Imports.iproute <&> (<> "IPv4")
  Params.IpV6StandardType ->
    requestImport Imports.iproute <&> (<> "IPv6")
