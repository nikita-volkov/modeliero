module Modeliero.Templates.RefinedArbitraryInstanceSpec where

import Coalmine.Prelude
import Modeliero.Dsls.InModule qualified as InModule
import Modeliero.Templates.RefinedArbitraryInstance qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Month" do
      let params =
            Subject.Params
              { name = "Month",
                type_ = Subject.IntType 1 12
              }

      producesExpectedContent params
        $ [i|
            module Z.V where
            
            import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
            import Test.QuickCheck.Gen qualified as QuickCheck.Gen

            instance QuickCheck.Arbitrary.Arbitrary Month where
              arbitrary = Month <$$> QuickCheck.Gen.choose (1, 12)
              shrink = const []
          |]

    describe "Phone" do
      let params =
            Subject.Params
              { name = "Phone",
                type_ = Subject.TextType 5 12
              }

      producesExpectedContent params
        $ [i|
            module Z.V where
            
            import Data.Text qualified as Text
            import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
            import Test.QuickCheck.Gen qualified as QuickCheck.Gen

            instance QuickCheck.Arbitrary.Arbitrary Phone where
              arbitrary = do
                length <- QuickCheck.Gen.chooseInt (5, 12)
                string <- QuickCheck.Gen.vectorOf length QuickCheck.Arbitrary.arbitrary
                pure (Phone (fromString string))
              shrink = \(Phone text) -> do
                toTake <- enumFromTo 5 (Text.length text)
                pure (Phone (Text.take toTake text))
          |]

producesExpectedContent :: Subject.Params -> Text -> Spec
producesExpectedContent params expectedResult = do
  it "Produces expected content" do
    let code = Subject.compile params
        codeText =
          InModule.compileContent
            ["Z", "V"]
            [ ("Test.QuickCheck.Gen", "QuickCheck.Gen"),
              ("Test.QuickCheck.Arbitrary", "QuickCheck.Arbitrary"),
              ("Data.Text", "Text")
            ]
            code
    shouldBe codeText expectedResult
