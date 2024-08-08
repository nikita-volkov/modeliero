{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegens.Haskell where

import Coalmine.NumericVersion qualified as NumericVersion
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
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule qualified as Templates.RefinedModelModule

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
        { name = ("modeliero-artifacts" <> params.name) & Slug.toSpinalCaseText,
          synopsis = "Generated model package",
          version = NumericVersion.fromNonEmpty params.version,
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
      [ "ModelieroArtifacts",
        params.name & Slug.toUpperCamelCaseText
      ]
    typesNamespace =
      rootNamespace <> ["Types"]
    importAliases =
      stdAliases <> modelAliases
      where
        stdAliases =
          [ ("Test.QuickCheck.Gen", "Qc.Gen"),
            ("Test.QuickCheck.Arbitrary", "Qc.Arbitrary"),
            ("Data.Text", "Text"),
            ("Data.Aeson", "Aeson"),
            ("Data.Aeson.Key", "Aeson.Key"),
            ("Data.Aeson.KeyMap", "Aeson.KeyMap"),
            ("Data.Aeson.Types", "Aeson.Types"),
            ("Prelude", "")
          ]
        modelAliases =
          params.types
            & fmap
              ( \type_ ->
                  ( (typesNamespace <> [type_.name & Slug.toUpperCamelCaseText])
                      & Text.intercalate ".",
                    "Local"
                  )
              )
    modelModules =
      params.types
        & fmap
          ( \type_ ->
              InModule.compileToModule
                (typesNamespace <> [type_.name & Slug.toUpperCamelCaseText])
                importAliases
                case type_.definition of
                  Params.ProductTypeDefinition fields ->
                    IntegratedTemplates.ProductModelModule.compile
                      typesNamespace
                      params.instances
                      type_.name
                      type_.docs
                      fields
                  Params.RefinedTypeDefinition refinement ->
                    Templates.RefinedModelModule.compile
                      Templates.RefinedModelModule.Params
                        { name = type_.name,
                          docs = type_.docs,
                          refinement,
                          instances = params.instances
                        }
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
