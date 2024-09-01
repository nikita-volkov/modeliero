module Modeliero.Sources.AsyncApi.TestModel.ByFeature.Anonymization.ByProperty.XAnonymizable.ByPosition.OutlineSchema.ByValue.EnabledSpec where

import Modeliero.Sources.AsyncApi.TestModel.Preludes.Spec

spec :: Spec
spec = do
  parsingSchema
    "Fixture 1"
    [i|
      asyncapi: 2.0.0
      info:
        title: Test
        version: 1.0.0
      
      components:
        schemas:
          email:
            type: string
            format: email
            x-anonymizable: True
    |]
    \case
      typeDeclarations -> do
        it "Has effect" do
          case find ((==) "email" . (.name)) typeDeclarations of
            Nothing -> expectationFailure "Not found"
            Just typeDeclaration -> case typeDeclaration.definition of
              NewtypeTypeDefinition newtypeDefinition ->
                shouldBe newtypeDefinition.anonymizable True
              _ ->
                expectationFailure "Unexpected type definition"
