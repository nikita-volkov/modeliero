{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegens.Haskell where

import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Dsls.Code qualified as Code
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
import Modeliero.Codegens.Haskell.Dsls.Namespace qualified as Namespace
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package
import Modeliero.Codegens.Haskell.IntegratedTemplates.ProductModelModule qualified as IntegratedTemplates.ProductModelModule
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Templates.ProductModelModule
import Modeliero.Codegens.Haskell.Templates.ReexportsModule qualified as Templates.ReexportsModule

type Params = Params.Model

-- | Compiled files of the package at their relative paths.
type Result = [(FilePath, Text)]

compile :: Params -> Result
compile params =
  Package.compilePackageFiles package
    & fmap (\file -> (file.path, file.content))
  where
    package =
      Package.Package
        { name = params.name & Slug.toSpinalCaseText,
          synopsis = "Generated model package",
          modules
        }
    modules =
      Package.Modules
        { public =
            [ reexportsModule
            ],
          private =
            modelModules
        }
    rootNamespace =
      [params.name & Slug.toUpperCamelCaseText]
    typesNamespace =
      rootNamespace <> ["Types"]
    importAliases = []
    modelModules =
      params.types
        & fmap
          ( \type_ ->
              let moduleName = typesNamespace <> [type_.name & Slug.toUpperCamelCaseText]
               in case type_.definition of
                    Params.ProductTypeDefinition fields ->
                      InModule.compileToModule moduleName importAliases
                        $ IntegratedTemplates.ProductModelModule.compile
                          typesNamespace
                          params.instances
                          type_.name
                          type_.docs
                          fields
          )
    reexportsModule =
      InModule.compileToModule rootNamespace importAliases
        $ Templates.ReexportsModule.compile
          Templates.ReexportsModule.Params
            { models =
                modelModules
                  & fmap (.name)
                  & fmap (Text.intercalate ".")
            }
