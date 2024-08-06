module Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.FromJsonInstanceSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.FromJsonInstance qualified as Subject
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
                        jsonName = "name",
                        nullable = False
                      },
                    Subject.Field
                      { haskellName = "genreName",
                        jsonName = "genre-name",
                        nullable = True
                      }
                  ]
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where

            import Data.Aeson qualified as Aeson
            import Data.Aeson.Types qualified as Aeson.Types
            
            instance Aeson.FromJSON Artist where
              parseJSON = \case
                Aeson.Object object -> do
                  name <- Aeson.Types.parseField object "name"
                  genreName <- Aeson.Types.parseFieldMaybe object "genre-name"
                  pure Artist{..}
                json -> Aeson.Types.typeMismatch "Object" json
          |]
