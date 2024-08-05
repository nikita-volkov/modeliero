module Modeliero.Codegens.Haskell.Templates.ProductArbitraryInstanceSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Templates.ProductArbitraryInstance qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Artist" do
      let params =
            Subject.Params
              { name = "Artist",
                fields =
                  [ "name",
                    "genre"
                  ]
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where
            
            import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary

            instance QuickCheck.Arbitrary.Arbitrary Artist where
              arbitrary Artist{..} = do
                name <- QuickCheck.Arbitrary.arbitrary
                genre <- QuickCheck.Arbitrary.arbitrary
                pure Artist{..}
              shrink value = do
                name <- QuickCheck.Arbitrary.shrink value.name
                genre <- QuickCheck.Arbitrary.shrink value.genre
                pure Artist{..}
          |]
