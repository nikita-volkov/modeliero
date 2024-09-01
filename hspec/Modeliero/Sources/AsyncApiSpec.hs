{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- |
-- A proper feature-oriented spec.
module Modeliero.Sources.AsyncApiSpec where

import Coalmine.Prelude
import Data.Text.IO qualified as Text
import Modeliero.Codegens.Haskell.Params
import Modeliero.Sources.AsyncApi qualified as Source
import Modeliero.Util.Specs
import System.Directory qualified as Directory
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "By feature" do
    describe "Anonymization" do
      describe "By property" do
        describe "x-anonymizable" do
          describe "By position" do
            describe "Outline schema" do
              describe "By value" do
                describe "Enabled" do
                  it "Has effect" do
                    pending

                describe "Disabled" do
                  it "Has effect" do
                    pending

            describe "Inline schema" do
              describe "By value" do
                describe "Enabled" do
                  it "Has no effect" do
                    pending

                describe "Disabled" do
                  it "Has no effect" do
                    pending

    describe "Optionality" do
      pure () -- TODO: To be modelled
    describe "Arrays" do
      pure () -- TODO: To be modelled
    describe "Dictionaries" do
      pure () -- TODO: To be modelled
    describe "Products" do
      pure () -- TODO: To be modelled
    describe "Sum-pattern 1" do
      pure () -- TODO: To be modelled
    describe "Enum" do
      pure () -- TODO: To be modelled
    describe "Inline products" do
      pure () -- TODO: To be modelled
    describe "Inline sums" do
      pure () -- TODO: To be modelled
    describe "Inline enums" do
      pure () -- TODO: To be modelled
