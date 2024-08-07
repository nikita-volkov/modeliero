{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegens.Haskell where

import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.Code qualified as Code
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
import Modeliero.Codegens.Haskell.Dsls.Namespace qualified as Namespace
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package
import Modeliero.Codegens.Haskell.IntegratedTemplates.ProductModelModule qualified as IntegratedTemplates.ProductModelModule
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Templates.ProductModelModule

type Params = Params.Model

-- | Compiled files of the package at their relative paths.
type Result = [(FilePath, Text)]

compile :: Params -> Result
compile params =
  error "TODO"
  where
    modelsNamespace =
      [ params.name & Slug.toUpperCamelCaseText,
        "Types"
      ]
    importAliases = []
    modelModules =
      params.types
        & fmap
          ( \type_ ->
              let moduleName = modelsNamespace <> [type_.name & Slug.toUpperCamelCaseText]
               in case type_.definition of
                    Params.ProductTypeDefinition fields ->
                      InModule.compileToModule moduleName importAliases
                        $ IntegratedTemplates.ProductModelModule.compile
                          modelsNamespace
                          params.instances
                          type_.name
                          type_.docs
                          fields
          )
