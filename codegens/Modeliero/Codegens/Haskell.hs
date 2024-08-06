{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegens.Haskell where

import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.Code qualified as Code
import Modeliero.Codegens.Haskell.Dsls.Namespace qualified as Namespace
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Templates.ProductModelModule

type Params = Params.Model

-- | Compiled files of the package at their relative paths.
type Result = [(FilePath, Text)]

compile :: Params -> Result
compile params =
  error "TODO"
  where
    modelModules =
      params.types
        & fmap
          ( \type_ ->
              case type_.definition of
                Params.ProductTypeDefinition fields ->
                  Templates.ProductModelModule.Params
                    { name = Slug.toUpperCamelCaseText type_.name,
                      haddock = type_.docs,
                      fields =
                        fields
                          & fmap (error "TODO"),
                      instances =
                        error "TODO"
                    }
          )
