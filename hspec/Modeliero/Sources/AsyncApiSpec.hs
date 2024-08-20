{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Sources.AsyncApiSpec where

import Coalmine.Prelude
import Data.Text.IO qualified as Text
import Modeliero.Sources.AsyncApi qualified as Source
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "asyncapi-1" do
    it "Loads fine" do
      Source.load "fixtures/asyncapi-1.yaml"
        >>= Text.putStrLn
        . renderAsYamlText
