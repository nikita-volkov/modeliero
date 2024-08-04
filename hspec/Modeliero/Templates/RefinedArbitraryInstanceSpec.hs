module Modeliero.Templates.RefinedArbitraryInstanceSpec where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Templates.RefinedArbitraryInstance qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Month" do
      let params =
            Subject.Params
              { name = "month",
                type_ = Subject.IntType 1 12
              }
      producesExpectedContent params
        $ [i|
            module Z.V where
            
            import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
            import Test.QuickCheck.Gen qualified as QuickCheck.Gen

            instance QuickCheck.Arbitrary.Arbitrary Month where
              arbitrary = Month <$$> QuickCheck.Gen.choose (1, 12)
          |]

producesExpectedContent :: Subject.Params -> Text -> Spec
producesExpectedContent params expectedResult = do
  it "Produces expected content" do
    let code = Subject.compile params
        codeText =
          Code.compileCodeContent
            ["Z", "V"]
            []
            code
    shouldBe codeText expectedResult
