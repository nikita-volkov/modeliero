module Modeliero.AsyncApiSpec where

import Coalmine.Prelude
import Modeliero.AsyncApi qualified as AsyncApi
import Test.Hspec

spec :: Spec
spec = do
  describe "asyncapi-1" do
    it "Loads fine" do
      asyncApiLoaded <-
        AsyncApi.load "hspec/Modeliero/AsyncApiSpec/fixtures/email-service.yaml"
          & try @SomeException
      case asyncApiLoaded of
        Left exception ->
          exception
            & displayException
            & expectationFailure
        Right _ -> pure ()
