{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

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
  fixture "email-service" \typeDeclarations -> do
    describe "user-signed-up-payload" do
      case find ((== "user-signed-up-payload") . (.name)) typeDeclarations of
        Nothing -> itFailsWithInput "Exists" typeDeclarations
        Just typeDeclaration -> do
          describe "Definition" do
            case typeDeclaration.definition of
              ProductTypeDefinition fields -> do
                describe "first-name" do
                  case find ((==) "first-name" . (.name)) fields of
                    Just field ->
                      case field.type_ of
                        MaybeValueType plainType ->
                          it "Is Text" do
                            shouldBe plainType
                              $ StandardPlainType
                              $ TextStandardType
                        _ -> itFailsWithInput "Is Maybe" field.type_
                    Nothing ->
                      itFailsWithInput "Exists" fields
                describe "email" do
                  case find ((==) "email" . (.name)) fields of
                    Nothing -> itFailsWithInput "Exists" fields
                    Just field -> do
                      case field.type_ of
                        PlainValueType plainType -> do
                          it "Is Email" do
                            shouldBe plainType
                              $ LocalPlainType "email"
                        _ -> itFailsWithInput "Is Plain" field.type_
              _ -> itFailsWithInput "Is product" typeDeclaration.definition

    describe "email" do
      case find ((==) "email" . (.name)) typeDeclarations of
        Nothing -> do
          itFailsWithInput "Exists" typeDeclarations
        Just typeDeclaration -> do
          describe "Definition" do
            case typeDeclaration.definition of
              ValueTypeDefinition valueType -> case valueType of
                PlainValueType plainType -> do
                  it "Is Email" do
                    shouldBe plainType
                      $ StandardPlainType
                      $ EmailStandardType
                _ -> itFailsWithInput "Is Plain" valueType

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

fixture :: String -> ([TypeDeclaration] -> Spec) -> Spec
fixture name cont =
  describe name do
    runIO (Source.load config filePath) >>= \case
      Left err ->
        err
          & renderAsYamlText
          & toList
          & expectationFailure
          & it "Loads fine"
      Right model -> cont model
  where
    filePath =
      fixturesPath <> "valid/" <> name <> ".yaml"

fixturesPath :: FilePath
fixturesPath =
  "hspec/Modeliero/Sources/AsyncApiSpec/fixtures/"

config :: Source.Config
config =
  Source.Config
    { defaultTextMaxLength = 10000
    }
