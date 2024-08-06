module Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ToJsonInstanceSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ToJsonInstance qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Artist" do
      let params =
            Subject.Params
              { typeName = "Artist",
                fields =
                  [ Subject.Field
                      { haskellName = "name",
                        jsonName = "name"
                      },
                    Subject.Field
                      { haskellName = "genreName",
                        jsonName = "genre-name"
                      }
                  ]
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where

            import Data.Aeson qualified as Aeson
            import Data.Aeson.KeyMap qualified as Aeson.KeyMap
            
            instance Aeson.ToJSON Artist where
              toJSON value =
                (Aeson.Object . Aeson.KeyMap.fromList)
                  [ ("name", Aeson.toJSON value.name),
                    ("genre-name", Aeson.toJSON value.genreName)
                  ]
          |]
