module Modeliero.Codegens.Haskell.Templates.ModelModule.Templates.ProductDataTypeDeclarationSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Templates.ModelModule.Templates.ProductDataTypeDeclaration qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Artist" do
      let params =
            Subject.Params
              { name = "Artist",
                haddock = "Some docs\nmore docs\n",
                fields =
                  [ Subject.Field
                      { name = "name",
                        type_ = "Text",
                        haddock = ""
                      },
                    Subject.Field
                      { name = "genreName",
                        type_ = "Text",
                        haddock = "Some docs\n"
                      }
                  ],
                derivings =
                  Subject.Derivings
                    { show = True,
                      eq = True,
                      ord = True,
                      generic = True
                    }
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where
            
            import Prelude
            import GHC.Generics qualified

            -- |
            -- Some docs
            -- more docs
            data Artist = Artist
              { name :: Text,
                -- | Some docs
                genreName :: Text
              }
              deriving (Show, Eq, Ord, GHC.Generics.Generic)
          |]
