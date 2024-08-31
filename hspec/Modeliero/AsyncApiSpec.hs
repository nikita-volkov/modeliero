module Modeliero.AsyncApiSpec where

import Coalmine.Prelude
import Modeliero.AsyncApi qualified as AsyncApi
import Test.Hspec

spec :: Spec
spec = do
  describe "asyncapi-1" do
    it "Loads fine" do
      asyncApi <- AsyncApi.load "hspec/Modeliero/AsyncApiSpec/fixtures/email-service.yaml"
      forM_ asyncApi.components.schemas \schema ->
        print schema
