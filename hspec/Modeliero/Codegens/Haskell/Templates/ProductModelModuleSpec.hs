module Modeliero.Codegens.Haskell.Templates.ProductModelModuleSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Templates.ProductModelModule qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Artist" do
      let params =
            Subject.Params
              { name = "Artist",
                haddock = "Docs on artist",
                fields =
                  [ Subject.Field
                      { name = "name",
                        type_ = "Text",
                        haddock = "Docs on name",
                        jsonName = "name",
                        nullable = False
                      },
                    Subject.Field
                      { name = "genreName",
                        type_ = "Text",
                        haddock = "",
                        jsonName = "genre-name",
                        nullable = True
                      }
                  ],
                instances =
                  Subject.Instances
                    { show = True,
                      eq = True,
                      ord = True,
                      generic = True,
                      aeson = True,
                      arbitrary = True
                    }
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V
              ( Artist(..),
              )
            where
            
            import Prelude
            import Data.Aeson qualified as Aeson
            import Data.Aeson.KeyMap qualified as Aeson.KeyMap
            import Data.Aeson.Types qualified as Aeson.Types
            import GHC.Generics qualified
            import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
            
            -- | Docs on artist
            data Artist = Artist
              { -- | Docs on name
                name :: Text,
                genreName :: Text
              }
              deriving (Show, Eq, Ord, GHC.Generics.Generic)
            
            instance Aeson.ToJSON Artist where
              toJSON value =
                (Aeson.Object . Aeson.KeyMap.fromList)
                  [ ("name", Aeson.toJSON value.name),
                    ("genre-name", Aeson.toJSON value.genreName)
                  ]
            
            instance Aeson.FromJSON Artist where
              parseJSON = \case
                Aeson.Object object -> do
                  name <- Aeson.Types.parseField object "name"
                  genreName <- Aeson.Types.parseFieldMaybe object "genre-name"
                  pure Artist{..}
                json -> Aeson.Types.typeMismatch "Object" json
            
            instance QuickCheck.Arbitrary.Arbitrary Artist where
              arbitrary Artist{..} = do
                name <- QuickCheck.Arbitrary.arbitrary
                genreName <- QuickCheck.Arbitrary.arbitrary
                pure Artist{..}
              shrink value = do
                name <- QuickCheck.Arbitrary.shrink value.name
                genreName <- QuickCheck.Arbitrary.shrink value.genreName
                pure Artist{..}
          |]
