module Modeliero.ExtrasFor.Hspec where

import Coalmine.Prelude
import Modeliero.Dsls.InModule qualified as InModule
import Test.Hspec

compilingProducesExpectedContent ::
  (params -> InModule.InModule TextBlock) ->
  params ->
  Text ->
  Spec
compilingProducesExpectedContent compile params expectedResult = do
  it "Produces expected content" do
    let code = compile params
        codeText =
          InModule.compileContent
            ["Z", "V"]
            [ ("Test.QuickCheck.Gen", "QuickCheck.Gen"),
              ("Test.QuickCheck.Arbitrary", "QuickCheck.Arbitrary"),
              ("Data.Text", "Text")
            ]
            code
    shouldBe codeText expectedResult
