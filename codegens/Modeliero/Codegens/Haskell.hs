module Modeliero.Codegens.Haskell where

import Coalmine.NumericVersion qualified as NumericVersion
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Templates.ProductModelModule
import Modeliero.Codegens.Haskell.Templates.ReexportsModule qualified as Templates.ReexportsModule
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule qualified as Templates.RefinedModelModule
import Modeliero.Codegens.Haskell.Templates.SumModelModule qualified as Templates.SumModelModule

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
          [ ("Data.Aeson.Key", "Aeson.Key"),
            ("Data.Aeson.KeyMap", "Aeson.KeyMap"),
            ("Data.Aeson.Types", "Aeson.Types"),
            ("Data.Aeson", "Aeson"),
            ("Data.Hashable", "Hashable"),
            ("Data.Scientific", "Scientific"),
            ("Data.Text", "Text"),
            ("GHC.Generics", "Generics"),
            ("ModelieroBase.Classes.Special", "Special"),
            ("Prelude", ""),
            ("Test.QuickCheck.Arbitrary", "QuickCheck.Arbitrary"),
            ("Test.QuickCheck.Gen", "QuickCheck.Gen")
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
                    Templates.ProductModelModule.compile
                      Templates.ProductModelModule.Params
                        { modelsNamespace = typesNamespace,
                          instances = params.instances,
                          name = type_.name,
                          docs = type_.docs,
                          fields
                        }
                  Params.RefinedTypeDefinition refinement ->
                    Templates.RefinedModelModule.compile
                      Templates.RefinedModelModule.Params
                        { name = type_.name,
                          docs = type_.docs,
                          refinement,
                          instances = params.instances
                        }
                  Params.SumTypeDefinition variants ->
                    Templates.SumModelModule.compile
                      Templates.SumModelModule.Params
                        { modelsNamespace = typesNamespace,
                          name = type_.name,
                          docs = type_.docs,
                          variants,
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
