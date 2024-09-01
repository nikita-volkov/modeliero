module Modeliero.Sources.AsyncApi.ValidInvalidFixturesSpec (spec) where

import Coalmine.Prelude
import Modeliero.Sources.AsyncApi qualified as Source
import System.Directory qualified as Directory
import Test.Hspec

spec :: Spec
spec = do
  describe "Valid" do
    let dirPath = fixturesPath <> "valid/"
    invalidFixturePaths <-
      Directory.listDirectory dirPath
        & runIO
    forM_ invalidFixturePaths \path -> describe path do
      it "Loads fine" do
        Source.load config (dirPath <> path) >>= \case
          Left err ->
            err
              & renderAsYamlText
              & toList
              & expectationFailure
          Right _ ->
            pure ()

  describe "Invalid" do
    let dirPath = fixturesPath <> "invalid/"
    invalidFixturePaths <-
      Directory.listDirectory dirPath
        & runIO
    forM_ invalidFixturePaths \path -> describe path do
      it "Fails to load" do
        Source.load config (dirPath <> path) >>= \case
          Left _err -> pure ()
          Right model ->
            model
              & renderAsYamlText
              & toList
              & expectationFailure

fixturesPath :: FilePath
fixturesPath =
  "hspec/Modeliero/Sources/AsyncApi/ValidInvalidFixturesSpec/fixtures/"

config :: Source.Config
config =
  Source.Config
    { defaultTextMaxLength = 10000
    }
