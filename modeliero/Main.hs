module Main where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell qualified as HaskellCodegen
import Modeliero.Codegens.Haskell.Params qualified as HaskellCodegen
import Modeliero.Sources.AsyncApi qualified as AsyncApiSource

main :: IO ()
main =
  AsyncApiSource.load AsyncApiSource.defaultConfig "asyncapi.yaml" >>= \case
    Left err ->
      err
        & renderAsYamlText
        & toList
        & fail
    Right source ->
      HaskellCodegen.Model
        { name = source.name,
          version = source.version,
          types = source.types,
          instances =
            HaskellCodegen.Instances
              { aeson =
                  Just
                    HaskellCodegen.Aeson
                      { casing = HaskellCodegen.KebabCasing
                      },
                arbitrary = True,
                anonymizable = True
              }
        }
        & HaskellCodegen.write "."
