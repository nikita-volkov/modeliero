module Modeliero.Codegens.Haskell.ExtrasFor.Hspec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule qualified as InModule
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
          InModule.compileToContent
            ["Z", "V"]
            [ ("Test.QuickCheck.Gen", "QuickCheck.Gen"),
              ("Test.QuickCheck.Arbitrary", "QuickCheck.Arbitrary"),
              ("Data.Text", "Text"),
              ("Data.Aeson", "Aeson"),
              ("Data.Aeson.Key", "Aeson.Key"),
              ("Data.Aeson.KeyMap", "Aeson.KeyMap"),
              ("Data.Aeson.Types", "Aeson.Types"),
              ("Prelude", "")
            ]
            code
    shouldBe codeText expectedResult
